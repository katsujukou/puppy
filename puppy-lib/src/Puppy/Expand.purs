-- | Resolving names, instantiating parameterised rules and folding away
-- | `%inline` rules -- everything between the syntax tree and a grammar an
-- | automaton can be built from.
-- |
-- | This runs in two passes, and the split matters.
-- |
-- | `validate` reads every declaration and every rule *template*, whether or
-- | not a start symbol can reach it, and collects everything wrong with the
-- | grammar as written. Checking only what gets generated would quietly accept
-- | a rule with an unknown symbol in it just because nothing happened to use
-- | that rule -- "not generated" and "not checked" are different things.
-- |
-- | Instantiation then runs demand-driven from the start symbols, so a
-- | parameterised rule produces exactly the instances the grammar asks for.
-- | Errors from this pass come one at a time: unlike the checks above, each one
-- | stops the work that would have found the next.
module Puppy.Expand
  ( ExpandError
  , expand
  , validate
  ) where

import Prelude

import Prim hiding (Symbol)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Common (joinWith, trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Puppy.Grammar (Action(..), Bound(..), Production, Symbol(..))
import Puppy.Grammar as Grammar
import Puppy.Names as Names
import Puppy.Syntax (ConflictDirective(..), Pos, Span, SymbolRef(..))
import Puppy.Syntax as Syntax

type ExpandError = { message :: String, span :: Span }

type Named = { name :: String, span :: Span }

-- | A nonterminal instance still waiting to be generated, and the place that
-- | asked for it.
type Pending = { ref :: SymbolRef, span :: Span }

nowhere :: Pos
nowhere = { offset: 0, line: 1, column: 1 }

-- | How deeply an instance's arguments may nest before expansion gives up.
-- |
-- | A rule that instantiates itself with a *larger* argument -- `grow(X)`
-- | asking for `grow(grow(X))` -- has no finite set of instances, and every
-- | instance has a name of its own, so nothing that remembers names can notice.
-- | Deciding this statically is the termination problem for polymorphic
-- | recursion; a depth ceiling is the pragmatic answer, and real grammars nest
-- | arguments a handful deep at most.
maxInstanceDepth :: Int
maxInstanceDepth = 50

-- | A backstop for runaway shapes the depth ceiling would not catch.
maxInstances :: Int
maxInstances = 10000

-- | How many productions inlining may produce.
-- |
-- | This is a separate ceiling from `maxInstances` because it bounds something
-- | else entirely: every `%inline` symbol in a production multiplies that
-- | production by the number of alternatives the inlined rule has, so a grammar
-- | with two rules -- one start symbol and one two-way `%inline` -- can still
-- | ask for a billion productions.
maxExpandedProductions :: Int
maxExpandedProductions = 50000

-- | How much rewriting inlining may do, counted in symbols scanned or copied.
-- |
-- | This bounds a different runaway from `maxExpandedProductions`. A rule with
-- | a single alternative multiplies nothing: a production mentioning it a
-- | thousand times still yields exactly one production, so a ceiling on the
-- | output never trips. What grows is the rewriting -- each splice reads and
-- | rebuilds the whole right-hand side, so a chain of them costs about the
-- | square of its length.
-- |
-- | It sits high enough that a grammar which multiplies out runs into the
-- | ceiling on productions first, since that one names the actual cause.
maxInlineWork :: Int
maxInlineWork = 10000000

--------------------------------------------------------------------------------
-- Names in scope
--------------------------------------------------------------------------------

type Env =
  { terminals :: Set String
  , rules :: Map String Syntax.Rule
  }

tokensOf :: Syntax.Grammar -> Array Syntax.TokenDecl
tokensOf syn = Syntax.declaredTokens syn.tokens

-- | Building this cannot fail. Anything wrong with the names it collects is
-- | reported by `validate`, which runs before any of it is used.
buildEnv :: Syntax.Grammar -> Env
buildEnv syn =
  { terminals: Set.fromFoldable (map _.name (tokensOf syn))
  , rules: Map.fromFoldable (map (\r -> Tuple r.name r) syn.rules)
  }

--------------------------------------------------------------------------------
-- Static validation
--------------------------------------------------------------------------------

-- | Every name after its first occurrence.
repeated :: Array Named -> Array Named
repeated = go Set.empty []
  where
  go seen acc items = case Array.uncons items of
    Nothing -> acc
    Just { head, tail }
      | Set.member head.name seen -> go seen (Array.snoc acc head) tail
      | otherwise -> go (Set.insert head.name seen) acc tail

duplicateErrors :: (String -> String) -> Array Named -> Array ExpandError
duplicateErrors message =
  map (\n -> { message: message n.name, span: n.span }) <<< repeated

plural :: Int -> String -> String
plural n word = show n <> " " <> word <> (if n == 1 then "" else "s")

arityMessage :: String -> Int -> Int -> String
arityMessage name expected given =
  "`" <> name <> "` takes " <> plural expected "parameter"
    <> " but was given "
    <> plural given "argument"

-- | Everything wrong with a grammar that can be seen without instantiating it.
-- | An empty result means the grammar is well-formed as written.
validate :: Syntax.Grammar -> Array ExpandError
validate syn = Array.concat
  [ duplicateErrors (\n -> "token `" <> n <> "` is declared more than once")
      (map named (tokensOf syn))
  , duplicateErrors (\n -> "rule `" <> n <> "` is declared more than once")
      (map named syn.rules)
  , clashErrors
  , duplicateErrors
      (\n -> "start symbol `" <> n <> "` is declared more than once")
      (map (\s -> { name: s.symbol, span: s.span }) syn.starts)
  , duplicateErrors (\n -> "`" <> n <> "` is given a type more than once")
      (map (\t -> { name: t.symbol, span: t.span }) syn.types)
  , duplicateErrors
      (\n -> "`" <> n <> "` is given a precedence more than once")
      precedenceNames
  , Array.concatMap undeclaredPrecedence precedenceNames
  , Array.concatMap unknownTyped syn.types
  , startErrors
  , Array.concatMap badTokenName (tokensOf syn)
  , Array.concatMap badStartName syn.starts
  , entryPointClashes
  , externalDeriveErrors
  , placeholderErrors
  , duplicatePatternErrors
  , Array.concatMap unknownDerive syn.derives
  , duplicateErrors
      (\n -> "`" <> n <> "` is derived more than once")
      (map (\d -> { name: d.name, span: d.span }) syn.derives)
  , inlineCycleErrors syn
  , Array.concatMap (validateRule env) syn.rules
  ]
  where
  env = buildEnv syn

  named :: forall r. { name :: String, span :: Span | r } -> Named
  named r = { name: r.name, span: r.span }

  clashErrors = Array.mapMaybe clash syn.rules
    where
    clash r
      | Set.member r.name env.terminals = Just
          { message: "`" <> r.name <> "` is declared as a token and as a rule"
          , span: r.span
          }
      | otherwise = Nothing

  precedenceNames = Array.concatMap
    (\d -> map (\t -> { name: t, span: d.span }) d.tokens)
    syn.precedences

  undeclaredPrecedence n
    | Set.member n.name env.terminals = []
    | otherwise =
        [ { message: "`" <> n.name
              <> "` is given a precedence but is not a declared token"
          , span: n.span
          }
        ]

  -- A token name becomes a data constructor, so it has to look like one.
  -- Upper case in both modes. Where Puppy writes the token type this is
  -- PureScript's rule for a constructor; where it does not, it is Puppy's own,
  -- and worth keeping -- it is what tells a terminal from a rule at a glance in
  -- a production, and an external token type costs nothing to name this way.
  badTokenName decl
    | Names.isConstructor decl.name = []
    | otherwise =
        [ { message: "token `" <> decl.name
              <> "` must begin with an upper-case letter; that is how a grammar tells a terminal from a rule"
          , span: decl.span
          }
        ]

  -- A start symbol becomes a top-level function in the generated module,
  -- beside the ones the generator writes itself.
  badStartName s
    | not (Names.isValue s.symbol) =
        [ { message: "start symbol `" <> s.symbol
              <> "` cannot be a PureScript value; it has to begin with a lower case letter and must not be a reserved word"
          , span: s.span
          }
        ]
    | Names.takenByGeneratedCode s.symbol =
        [ { message: "start symbol `" <> s.symbol
              <> "` is a name the generated parser already uses"
          , span: s.span
          }
        ]
    | otherwise = []

  -- Each start symbol produces two top-level names: an entry point of its own
  -- name, and one with `From` appended that pulls its tokens. Two start
  -- symbols can therefore collide without either having been declared twice,
  -- which is what the duplicate check above would have caught.
  entryPointClashes =
    _.errors (Array.foldl declare { taken: Map.empty, errors: [] } syn.starts)
    where
    declare acc s = Array.foldl (claim s) acc
      [ s.symbol, Names.streamingName s.symbol ]

    claim s acc name = case Map.lookup name acc.taken of
      Just owner
        | owner.symbol /= s.symbol ->
            acc { errors = Array.snoc acc.errors (clashes s owner name) }
      _ ->
        acc { taken = Map.insert name { symbol: s.symbol, span: s.span } acc.taken }

    clashes s owner name =
      { message: "start symbols `" <> s.symbol <> "` and `" <> owner.symbol
          <> "` (line "
          <> show owner.span.start.line
          <> ", column "
          <> show owner.span.start.column
          <> ") both produce an entry point called `"
          <> name
          <> "`; each start symbol produces one of its own name and one with `From` appended"
      , span: s.span
      }

  externalTokens = case syn.tokens of
    Syntax.GeneratedTokens _ -> []
    Syntax.ExternalTokens spec -> spec.tokens

  isExternal = case syn.tokens of
    Syntax.GeneratedTokens _ -> false
    Syntax.ExternalTokens _ -> true

  -- An instance has to live in the module its type does, and with
  -- `%tokentype` that module is not this one.
  externalDeriveErrors
    | isExternal = map
        ( \d ->
            { message: "`" <> d.name
                <> "` cannot be derived here: with `%tokentype` the token type is yours, and an instance belongs in the module its type was declared in"
            , span: d.span
            }
        )
        syn.derives
    | otherwise = []

  -- `$$` is where the payload sits, so a token that carries one needs exactly
  -- one, and a token that carries nothing has nowhere to put it.
  placeholderErrors = Array.concatMap holes externalTokens
    where
    holes t = case t.decl.payload, Array.length t.pattern.holes of
      Nothing, 0 -> []
      Nothing, _ ->
        [ { message: "token `" <> t.decl.name
              <> "` carries no value, so there is nothing for `$$` to stand for; give it a `{ ... }` payload type or take the placeholder out"
          , span: t.pattern.code.span
          }
        ]
      Just _, 1 -> []
      Just _, 0 ->
        [ { message: "token `" <> t.decl.name
              <> "` carries a value, so its pattern needs a `$$` to say where in the token that value sits"
          , span: t.pattern.code.span
          }
        ]
      Just _, n ->
        [ { message: "token `" <> t.decl.name <> "` has " <> show n
              <> " `$$` placeholders; a token carries one value, so exactly one of them can be it"
          , span: t.pattern.code.span
          }
        ]

  -- Two patterns that are the same text, once the space a `{ ... }` leaves
  -- around them is gone, are the same pattern, and the second can never match.
  --
  -- Trimming is as far as this goes, and deliberately. Squeezing the space out
  -- of the middle as well would call `T.Text "a b"` and `T.Text "ab"` the same
  -- pattern, which they are not, and rejecting a grammar that is fine is worse
  -- than missing a duplicate the compiler will report as an unreachable arm.
  -- Everything subtler -- one pattern covering another -- is a question about
  -- PureScript patterns that Puppy is in no position to answer.
  duplicatePatternErrors =
    _.errors (Array.foldl claim { seen: Map.empty, errors: [] } externalTokens)
    where
    claim acc t = case Map.lookup (squashed t) acc.seen of
      Just owner -> acc { errors = Array.snoc acc.errors (clash t owner) }
      Nothing -> acc { seen = Map.insert (squashed t) t.decl.name acc.seen }

    clash t owner =
      { message: "token `" <> t.decl.name <> "` has the same pattern as `"
          <> owner
          <> "`; the first of two identical patterns takes everything that matches and the second is never reached"
      , span: t.pattern.code.span
      }

    squashed t = trim t.pattern.code.text

  -- Only the classes whose demands on a payload type can be stated. A token
  -- may carry something with no `Eq` at all, which is why these are asked for
  -- rather than assumed.
  unknownDerive d
    | Array.elem d.name [ "Eq", "Show" ] = []
    | otherwise =
        [ { message: "`" <> d.name
              <> "` cannot be derived for the token type; Puppy knows how to derive `Eq` and `Show`"
          , span: d.span
          }
        ]

  unknownTyped t
    | Map.member t.symbol env.rules = []
    | otherwise =
        [ { message: "`" <> t.symbol <> "` is given a type but has no rule"
          , span: t.span
          }
        ]

  startErrors
    | Array.null syn.starts =
        [ { message: "the grammar declares no `%start` symbol"
          , span: { start: nowhere, end: nowhere }
          }
        ]
    | otherwise = Array.concatMap checkStart syn.starts

  checkStart s = case Map.lookup s.symbol env.rules of
    Nothing ->
      [ { message: "start symbol `" <> s.symbol <> "` has no rule"
        , span: s.span
        }
      ]
    Just rule
      | not (Array.null rule.parameters) ->
          [ { message: "start symbol `" <> s.symbol
                <> "` cannot take parameters"
            , span: s.span
            }
          ]
      | rule.inline ->
          [ { message: "start symbol `" <> s.symbol
                <> "` cannot be `%inline`; there would be nothing left to start from"
            , span: s.span
            }
          ]
      | otherwise -> []

-- | A `%inline` rule that reaches itself has no order in which to be folded
-- | away, so there is no grammar to build.
-- |
-- | `orderInline` catches this too, but only among the instances a start symbol
-- | reaches, which lets an unused rule smuggle a cycle past. That check stays
-- | all the same: a cycle can also first appear once parameters are
-- | substituted, which is invisible here.
-- |
-- | Only the head of a right-hand side symbol is an edge. A head that is one of
-- | the rule's own parameters stands for something not known until
-- | instantiation, and passing a rule as an argument is not using it.
inlineCycleErrors :: Syntax.Grammar -> Array ExpandError
inlineCycleErrors syn = Array.mapMaybe check inlineRules
  where
  inlineRules = Array.filter _.inline syn.rules

  inlineNames = Set.fromFoldable (map _.name inlineRules)

  edges = Map.fromFoldable (map (\r -> Tuple r.name (uses r)) inlineRules)

  uses rule =
    let
      params = Set.fromFoldable rule.parameters

      headName el = case el.symbol of
        SymbolRef n _ -> n
    in
      Array.nub
        ( Array.filter
            (\n -> Set.member n inlineNames && not (Set.member n params))
            (Array.concatMap (\p -> map headName p.elements) rule.productions)
        )

  reachesSelf start = go (fromMaybe [] (Map.lookup start edges)) Set.empty
    where
    go frontier visited = case Array.uncons frontier of
      Nothing -> false
      Just { head, tail }
        | head == start -> true
        | Set.member head visited -> go tail visited
        | otherwise -> go
            (tail <> fromMaybe [] (Map.lookup head edges))
            (Set.insert head visited)

  check rule
    | reachesSelf rule.name = Just
        { message: "`" <> rule.name
            <> "` is `%inline` but reaches itself, directly or through another `%inline` rule, so there is no order in which to fold it away"
        , span: rule.span
        }
    | otherwise = Nothing

validateRule :: Env -> Syntax.Rule -> Array ExpandError
validateRule env rule = Array.concat
  [ duplicateErrors
      (\n -> "parameter `" <> n <> "` is declared more than once")
      (map (\p -> { name: p, span: rule.span }) rule.parameters)
  , Array.concatMap (validateProduction env params) rule.productions
  ]
  where
  params = Set.fromFoldable rule.parameters

validateProduction
  :: Env -> Set String -> Syntax.Production -> Array ExpandError
validateProduction env params prod = Array.concat
  [ duplicateErrors (\n -> "binder `" <> n <> "` is declared more than once")
      binderNames
  , precErrors
  , Array.concatMap badBinder binderNames
  , Array.concatMap
      (\el -> validateRef env params true el.span el.symbol)
      prod.elements
  ]
  where
  binderNames = Array.mapMaybe
    (\e -> map (\b -> { name: b, span: e.span }) e.binder)
    prod.elements

  -- A binder becomes a `let` binding around the author's own code.
  badBinder n
    | Names.isValue n.name = []
    | otherwise =
        [ { message: "binder `" <> n.name
              <> "` cannot be a PureScript value; it has to begin with a lower case letter and must not be a reserved word"
          , span: n.span
          }
        ]

  precErrors = case prod.directive of
    Prec name
      | not (Set.member name env.terminals) ->
          [ { message: "`%prec " <> name
                <> "` does not name a declared token"
            , span: prod.span
            }
          ]
    _ -> []

-- | Check that a reference names something that exists.
-- |
-- | `applied` says whether this position demands a fully applied symbol. It
-- | holds for a symbol on a right-hand side, and not for an argument: an
-- | argument may be a rule handed over for someone else to apply, as `twice` is
-- | in `apply(twice, A)`, so its arity is only knowable once it is used.
-- |
-- | A parameter stands for whatever it is instantiated with, so nothing about
-- | its arity can be checked here either.
validateRef
  :: Env -> Set String -> Boolean -> Span -> SymbolRef -> Array ExpandError
validateRef env params applied span (SymbolRef name args) =
  headErrors <> Array.concatMap (validateRef env params false span) args
  where
  headErrors
    | Set.member name params = []
    | Set.member name env.terminals =
        if Array.null args then []
        else
          [ { message: "`" <> name <> "` is a token and cannot take arguments"
            , span
            }
          ]
    | otherwise = case Map.lookup name env.rules of
        Nothing -> [ { message: "unknown symbol `" <> name <> "`", span } ]
        Just rule
          | applied && Array.length rule.parameters /= Array.length args ->
              [ { message: arityMessage name (Array.length rule.parameters)
                    (Array.length args)
                , span
                }
              ]
          | otherwise -> []

--------------------------------------------------------------------------------
-- Instantiating parameterised rules
--------------------------------------------------------------------------------

-- | Replace parameters by their arguments, throughout.
-- |
-- | A parameter can itself be applied -- `f(X): | X(Y) { ... }` -- so an
-- | argument's own arguments come first and the ones written at the use site
-- | follow.
substitute :: Map String SymbolRef -> SymbolRef -> SymbolRef
substitute subst (SymbolRef name args) =
  let
    args' = map (substitute subst) args
  in
    case Map.lookup name subst of
      Just (SymbolRef actual inner) -> SymbolRef actual (inner <> args')
      Nothing -> SymbolRef name args'

-- | The name an instance is known by. Instances are keyed on this, so it has to
-- | be built from arguments that are already substituted.
renderRef :: SymbolRef -> String
renderRef (SymbolRef name args)
  | Array.null args = name
  | otherwise = name <> "(" <> joinWith "," (map renderRef args) <> ")"

-- | How deeply a reference nests, giving up once past `limit` so that measuring
-- | a runaway grammar cannot itself overflow the stack.
depthAtMost :: Int -> SymbolRef -> Int
depthAtMost limit (SymbolRef _ args)
  | limit <= 0 = 0
  | otherwise = 1 + fromMaybe 0 (maximum (map (depthAtMost (limit - 1)) args))

type Resolved =
  { symbol :: Symbol
  , pending :: Maybe Pending
  }

resolveElement
  :: Env
  -> Map String SymbolRef
  -> Syntax.Element
  -> Either ExpandError Resolved
resolveElement env subst el = case substitute subst el.symbol of
  SymbolRef name args
    | Set.member name env.terminals ->
        if Array.null args then Right { symbol: Terminal name, pending: Nothing }
        else Left
          { message: "`" <> name <> "` is a token and cannot take arguments"
          , span: el.span
          }
    | otherwise -> case Map.lookup name env.rules of
        Nothing -> Left
          { message: "unknown symbol `" <> name <> "`"
          , span: el.span
          }
        Just rule
          | Array.length rule.parameters /= Array.length args -> Left
              { message: arityMessage name (Array.length rule.parameters)
                  (Array.length args)
              , span: el.span
              }
          | otherwise ->
              let
                ref = SymbolRef name args
              in
                Right
                  { symbol: Nonterminal (renderRef ref)
                  , pending: Just { ref, span: el.span }
                  }

instantiateProduction
  :: Env
  -> Map String SymbolRef
  -> String
  -> Syntax.Production
  -> Either ExpandError { production :: Production, pending :: Array Pending }
instantiateProduction env subst lhs sp = do
  resolved <- traverse (resolveElement env subst) sp.elements
  pure
    { production:
        { lhs
        , rhs: map _.symbol resolved
        , directive: sp.directive
        , action: Action { bindings, code: sp.action }
        , span: sp.span
        }
    , pending: Array.mapMaybe _.pending resolved
    }
  where
  bindings = Array.mapMaybe binding (Array.mapWithIndex Tuple sp.elements)

  binding (Tuple i e) = map (\b -> { name: b, value: FromStack i }) e.binder

-- | Generate every instance the start symbols reach, and note which of them
-- | came from a rule marked `%inline`.
generate
  :: Env
  -> Array Pending
  -> Either ExpandError { productions :: Array Production, inlines :: Set String }
generate env start = go (List.fromFoldable start) Set.empty 0 Nil Set.empty
  where
  go queue seen count acc inlines = case queue of
    Nil -> Right
      { productions: Array.fromFoldable (List.reverse acc)
      , inlines
      }
    Cons next rest -> case next.ref of
      SymbolRef head args
        | depthAtMost (maxInstanceDepth + 1) next.ref > maxInstanceDepth -> Left
            { message: "`" <> head
                <> "` is instantiated with arguments nested more than "
                <> show maxInstanceDepth
                <> " deep; a parameterised rule that uses itself with a larger argument, as `grow(X)` does in `grow(grow(X))`, has no finite set of instances"
            , span: next.span
            }
        | otherwise ->
            let
              name = renderRef next.ref
            in
              if Set.member name seen then go rest seen count acc inlines
              else if count >= maxInstances then Left
                { message: "expanding this grammar produced more than "
                    <> plural maxInstances "rule instance"
                    <> "; a parameterised rule is most likely instantiating itself with ever-changing arguments"
                , span: next.span
                }
              else case Map.lookup head env.rules of
                Nothing -> Left
                  { message: "unknown symbol `" <> head <> "`", span: next.span }
                Just rule
                  | Array.length rule.parameters /= Array.length args -> Left
                      { message: arityMessage head (Array.length rule.parameters)
                          (Array.length args)
                      , span: next.span
                      }
                  | otherwise ->
                      let
                        subst = Map.fromFoldable
                          (Array.zipWith Tuple rule.parameters args)
                      in
                        -- Deliberately a `case` and not a `do`: under a bind
                        -- the call below stops being a tail call, and the
                        -- worklist would then cost one stack frame per rule
                        -- instance rather than none.
                        case
                          traverse (instantiateProduction env subst name)
                            rule.productions
                          of
                          Left err -> Left err
                          Right results -> go
                            ( List.fromFoldable
                                (Array.concatMap _.pending results) <> rest
                            )
                            (Set.insert name seen)
                            (count + 1)
                            (Array.foldl (\a r -> r.production : a) acc results)
                            ( if rule.inline then Set.insert name inlines
                              else inlines
                            )

--------------------------------------------------------------------------------
-- Folding away %inline rules
--------------------------------------------------------------------------------

-- | Rewrite every stack reference in an action, nested ones included.
mapStack :: (Int -> Bound) -> Action -> Action
mapStack f (Action a) = Action a { bindings = map remap a.bindings }
  where
  remap b = b
    { value = case b.value of
        FromStack i -> f i
        FromAction inner -> FromAction (mapStack f inner)
    }

-- | Splice one alternative of an inlined rule into the position it was used at.
-- |
-- | Positions after the splice move by the difference between the width of what
-- | was inserted and the one symbol it replaced -- backwards, when the
-- | alternative is empty. The position itself stops being a stack slot at all
-- | and becomes the inlined rule's own action, now reading the slots it was
-- | spliced into.
inlineAt :: Int -> Production -> Production -> Production
inlineAt at outer inner = outer
  { rhs =
      Array.slice 0 at outer.rhs <> inner.rhs <> Array.drop (at + 1) outer.rhs
  -- The same rule `%prec` has always followed here, and `%shift` follows it
  -- too: what the use site said about a conflict wins, and an inlined rule
  -- lends what it said to a use site that said nothing.
  , directive = case outer.directive of
      Inferred -> inner.directive
      settled -> settled
  , action = mapStack replace outer.action
  }
  where
  width = Array.length inner.rhs

  replace i
    | i < at = FromStack i
    | i == at = FromAction (mapStack (\j -> FromStack (j + at)) inner.action)
    | otherwise = FromStack (i + width - 1)

groupByLhs :: Array Production -> Map String (Array Production)
groupByLhs = Array.foldl step Map.empty
  where
  step m p = Map.alter (Just <<< maybe [ p ] (_ <> [ p ])) p.lhs m

-- | Inline rules in an order where each comes after everything it uses, so that
-- | by the time one is spliced anywhere it contains no inline symbols itself.
orderInline
  :: Set String
  -> Map String (Array Production)
  -> Either ExpandError (Array String)
orderInline inlines byLhs = do
  st <- Array.foldl step (Right { done: Set.empty, order: Nil }) names
  pure (Array.fromFoldable (List.reverse st.order))
  where
  names = Set.toUnfoldable inlines :: Array String

  step acc name = acc >>= visit Set.empty name

  dependencies name = Array.nub
    ( Array.mapMaybe pick
        (Array.concatMap _.rhs (fromMaybe [] (Map.lookup name byLhs)))
    )

  pick = case _ of
    Nonterminal n | Set.member n inlines -> Just n
    _ -> Nothing

  spanOf name = case Map.lookup name byLhs >>= Array.head of
    Just p -> p.span
    Nothing -> { start: nowhere, end: nowhere }

  visit path name st
    | Set.member name st.done = Right st
    | Set.member name path = Left
        { message: "`" <> name
            <> "` is `%inline` but reaches itself, directly or through another `%inline` rule, so there is no order in which to fold it away"
        , span: spanOf name
        }
    | otherwise = do
        st' <- Array.foldl
          (\acc d -> acc >>= visit (Set.insert name path) d)
          (Right st)
          (dependencies name)
        pure { done: Set.insert name st'.done, order: name : st'.order }

type Expansion =
  { left :: Int
  , work :: Int
  , acc :: List Production
  }

-- | Expand a batch of productions against the inline definitions, threading one
-- | pair of budgets through all of them.
-- |
-- | Pending work is an explicit list rather than the call stack. Substituting
-- | one inline symbol can uncover another, and a production that mentions a
-- | thousand of them chains a thousand deep; recursing would run out of stack
-- | long before either budget noticed.
expandBatch
  :: Map String (Array Production)
  -> Array Production
  -> { left :: Int, work :: Int }
  -> Either ExpandError Expansion
expandBatch defs ps budget =
  go (List.fromFoldable ps) { left: budget.left, work: budget.work, acc: Nil }
  where
  isInline = case _ of
    Nonterminal n -> Map.member n defs
    _ -> false

  alternatives = case _ of
    Nonterminal n -> Map.lookup n defs
    _ -> Nothing

  -- The leftmost inline symbol, with what it can be replaced by.
  splice p = do
    at <- Array.findIndex isInline p.rhs
    symbol <- Array.index p.rhs at
    alts <- alternatives symbol
    pure { at, alts }

  -- One symbol scanned or copied is one unit. Finding the symbol reads the
  -- whole right-hand side, and every alternative rebuilds it, so a splice costs
  -- roughly the length of what it works on rather than a flat one.
  costOf p alts = Array.foldl
    (\n a -> n + Array.length p.rhs + Array.length a.rhs)
    (Array.length p.rhs)
    alts

  go pending st = case pending of
    Nil -> Right st
    Cons p rest -> case splice p of
      Nothing
        | st.left <= 0 -> Left
            { message: "inlining produced more than "
                <> plural maxExpandedProductions "production"
                <> "; every `%inline` symbol multiplies a production by the number of alternatives the inlined rule has, so a few dozen uses multiply out past any workable size"
            , span: p.span
            }
        | otherwise -> go rest st { left = st.left - 1, acc = p : st.acc }
      Just s ->
        let
          cost = costOf p s.alts
        in
          if st.work < cost then Left
            { message: "inlining did more than "
                <> show maxInlineWork
                <> " units of work"
                <> "; a production that inlines a long chain of rules is rewritten end to end at every step, which costs far more than the productions it finally yields"
            , span: p.span
            }
          else go
            (Array.foldr (\a queue -> inlineAt s.at p a : queue) rest s.alts)
            st { work = st.work - cost }

runInline :: Set String -> Array Production -> Either ExpandError (Array Production)
runInline inlines prods = do
  order <- orderInline inlines byLhs
  built <- Array.foldl define
    ( Right
        { defs: Map.empty
        , left: maxExpandedProductions
        , work: maxInlineWork
        }
    )
    order
  final <- expandBatch built.defs reachable { left: built.left, work: built.work }
  pure (Array.fromFoldable (List.reverse final.acc))
  where
  byLhs = groupByLhs prods

  reachable = Array.filter (\p -> not (Set.member p.lhs inlines)) prods

  -- Inline definitions are expanded against the ones already finished, and out
  -- of the same budgets: a definition that runs away is just as fatal as a use
  -- site that does.
  define acc name = acc >>= \st -> do
    expansion <- expandBatch st.defs
      (fromMaybe [] (Map.lookup name byLhs))
      { left: st.left, work: st.work }
    pure
      { defs: Map.insert name
          (Array.fromFoldable (List.reverse expansion.acc))
          st.defs
      , left: expansion.left
      , work: expansion.work
      }

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Instantiate and inline. A top-level function rather than a `where` binding
-- | of `expand`, so that none of this runs when validation has already failed:
-- | a `where` binding that is not a function is evaluated as soon as the
-- | enclosing function is entered, whichever branch is taken.
build :: Syntax.Grammar -> Either ExpandError Grammar.Grammar
build syn = do
  generated <- generate (buildEnv syn)
    (map (\s -> { ref: SymbolRef s.symbol [], span: s.span }) syn.starts)
  productions <- runInline generated.inlines generated.productions
  pure
    { header: syn.header
    , tokens: syn.tokens
    , precedences: syn.precedences
    , types: syn.types
    , derives: syn.derives
    , starts: syn.starts
    , productions
    }

expand :: Syntax.Grammar -> Either (Array ExpandError) Grammar.Grammar
expand syn = case validate syn of
  errors
    | not (Array.null errors) -> Left errors
    | otherwise -> case build syn of
        Left err -> Left [ err ]
        Right g -> Right g
