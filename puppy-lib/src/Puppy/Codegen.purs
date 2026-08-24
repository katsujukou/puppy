-- | Writing out the parser.
-- |
-- | The generator's job is to say faithfully what the tables already decided.
-- | It does not judge the grammar: a table with conflicts in it still becomes a
-- | parser, and whether that should stop the build is a question for whoever
-- | called the generator.
-- |
-- | Everything lands in one module. Splitting the token type out so that a
-- | lexer can be recompiled without the tables is worth doing eventually, which
-- | is why `tokenType` is its own function -- but a second module is a second
-- | name to invent and a second thing to keep in step, and neither pays for
-- | itself yet.
module Puppy.Codegen
  ( Input
  , Generated
  , generate
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Char (toCharCode)
import Data.Int (hexadecimal, toStringAs)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst)
import Puppy.Codegen.Emit (Emitter, SourceMapping)
import Puppy.Codegen.Emit as Emit
import Puppy.Grammar as Core
import Puppy.LR.Automaton (Automaton)
import Puppy.LR.Grammar (LRGrammar, Prod, Sym(..))
import Puppy.LR.Table (Action(..), Table)
import Puppy.Names as Names
import Puppy.Syntax (Code, TokenDecl, eofToken)
import Puppy.Syntax as Syntax

type Input =
  { moduleName :: String
  , core :: Core.Grammar
  , grammar :: LRGrammar
  , automaton :: Automaton
  , table :: Table
  }

type Generated =
  { source :: String
  , mappings :: Array SourceMapping
  }

--------------------------------------------------------------------------------
-- What the grammar said about types
--------------------------------------------------------------------------------

-- | The declared type of each nonterminal, from `%type` and `%start`.
declaredTypes :: Input -> Map String Code
declaredTypes input = Map.fromFoldable
  ( map (\t -> Tuple t.symbol t.resultType) input.core.types
      <> map (\s -> Tuple s.symbol s.resultType) input.core.starts
  )

tokenDecls :: Input -> Array TokenDecl
tokenDecls input = Syntax.declaredTokens input.core.tokens

-- | How the generated module names the token type.
-- |
-- | `Token` when Puppy wrote it, and the author's own type when it did not.
-- | Parenthesised in that case because it goes where an argument goes and may
-- | be more than one word.
tokenRef :: Input -> Emitter -> Emitter
tokenRef input acc = case input.core.tokens of
  Syntax.GeneratedTokens _ -> Emit.write "Token" acc
  Syntax.ExternalTokens spec ->
    Emit.write "(" acc # writeFragment 2 spec.tokenType # Emit.write ")"

-- | The patterns that pick each terminal out of an external token type, in
-- | declaration order. Empty when Puppy wrote the type itself.
tokenPatterns :: Input -> Array Syntax.TokenPattern
tokenPatterns input = case input.core.tokens of
  Syntax.GeneratedTokens _ -> []
  Syntax.ExternalTokens spec -> map _.pattern spec.tokens

external :: Input -> Boolean
external input = case input.core.tokens of
  Syntax.GeneratedTokens _ -> false
  Syntax.ExternalTokens _ -> true

-- | The type a symbol's semantic value has, where the grammar said.
typeOfSymbol :: Input -> Sym -> Maybe Code
typeOfSymbol input = case _ of
  T t -> Array.index (tokenDecls input) t >>= _.payload
  N n -> Array.index input.grammar.nonterminals n
    >>= \name -> Map.lookup name (declaredTypes input)

--------------------------------------------------------------------------------
-- Names the generator introduces
--------------------------------------------------------------------------------

-- | Every name bound anywhere in an action tree, including the actions of
-- | rules that were inlined into it.
boundNames :: Core.Action -> Array String
boundNames (Core.Action a) = Array.concatMap names a.bindings
  where
  names b = Array.cons b.name case b.value of
    Core.FromAction inner -> boundNames inner
    Core.FromStack _ -> []

-- | A name for the array of semantic values that no binding in the tree uses.
-- |
-- | The array is in scope throughout, including inside the `let` an inlined
-- | rule becomes, so a binding that happened to share its name would capture
-- | it -- and that binding may be one the grammar author wrote.
argumentName :: Core.Action -> String
argumentName action = go 0
  where
  taken = boundNames action

  go n =
    let
      candidate = if n == 0 then "puppyValues" else "puppyValues" <> show n
    in
      if Array.elem candidate taken then go (n + 1) else candidate

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

spaces :: Int -> String
spaces n = joinWith "" (Array.replicate n " ")

line :: Int -> String -> String
line indent text = spaces indent <> text <> "\n"

-- | A PureScript string literal, or an expression building one.
-- |
-- | Display names come from the grammar, and the grammar's own lexer is happy
-- | to let a real newline sit inside one. A newline written straight into a
-- | literal does not compile, so every character that cannot appear as itself
-- | is escaped -- including the control characters nobody meant to type.
-- |
-- | A `\x` escape is greedy and PureScript has no `\&` to stop it with, so a
-- | hex digit following one would be swallowed into it. Where that would
-- | happen the literal is closed and a new one concatenated, which reads
-- | oddly and is at least correct.
quoted :: String -> String
quoted text = "\"" <> joinWith "" (Array.mapWithIndex piece chars) <> "\""
  where
  chars = SCU.toCharArray text

  piece i c = escape c <> separator i c

  -- Only after a `\x`. The named escapes have a fixed length and end where
  -- they end.
  separator i c =
    if hexEscaped c && maybe false isHexDigit (Array.index chars (i + 1)) then
      "\" <> \""
    else ""

  hexEscaped c = isControl c && c /= '\n' && c /= '\r' && c /= '\t'

  escape c = case c of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | isControl c -> "\\x" <> padded (toStringAs hexadecimal (toCharCode c))
      | otherwise -> SCU.singleton c

  isControl c = toCharCode c < 32 || toCharCode c == 127

  -- Two digits always: `\x1` is legal but leaves the escape open to whatever
  -- comes next, and padding costs nothing.
  padded digits = if SCU.length digits < 2 then "0" <> digits else digits

  isHexDigit c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Write an author's fragment, keeping its shape but moving it under the
-- | indentation it now sits at. Lines after the first keep whatever they had
-- | relative to each other, which is what the layout rule cares about.
writeFragment :: Int -> Code -> Emitter -> Emitter
writeFragment = Emit.writeCode

--------------------------------------------------------------------------------
-- Semantic actions
--------------------------------------------------------------------------------

-- | One semantic action, as a function from the values a production reduced
-- | over to the value it produces.
semanticAction :: Input -> Int -> Int -> Prod -> Emitter -> Emitter
semanticAction input indent index prod e = case prod.source >>= sourceAction of
  Nothing ->
    -- The productions added for the start symbols are never reduced by; the
    -- driver accepts instead.
    Emit.write
      ( "\\_ -> Puppy.Runtime.internalError\n"
          <> line indent (quoted "the start production has no semantic action")
      )
      e
  Just action ->
    Emit.write ("\\" <> parameterFor action <> " ->\n") e
      # scoped input indent (inlinePrefix index) prod
          (Just (resultTypeOf prod))
          true
          action
  where
  sourceAction i = Array.index input.core.productions i # map _.action

  resultTypeOf p = Array.index input.grammar.nonterminals p.lhs
    >>= \name -> Map.lookup name (declaredTypes input)

-- | An action that binds nothing never mentions the values, and a named
-- | parameter nobody uses is a warning in whatever project this lands in.
parameterFor :: Core.Action -> String
parameterFor action@(Core.Action a) =
  if Array.null a.bindings then "_" else argumentName action

inlinePrefix :: Int -> String
inlinePrefix index = "puppyInline" <> show index

-- | An action and the bindings it runs under.
scoped
  :: Input
  -> Int
  -> String
  -> Prod
  -> Maybe (Maybe Code)
  -> Boolean
  -> Core.Action
  -> Emitter
  -> Emitter
scoped input indent path prod annotation boxed action@(Core.Action a) e =
  if Array.null a.bindings then body indent e
  else
    Emit.write (line indent "let") e
      # bindings (indent + 2)
      # Emit.write (line indent "in")
      # body (indent + 2)
  where
  values = parameterFor action

  bindings at start = Array.foldl (binding at) start
    (Array.mapWithIndex Tuple a.bindings)

  -- A blank line between bindings, which is how a formatter would leave them
  -- once each carries a type signature.
  binding at acc (Tuple i b) =
    let
      spaced = if i == 0 then acc else Emit.write "\n" acc
    in
      case b.value of
        Core.FromStack index ->
          spaced
            # annotate at b.name
                (Array.index prod.rhs index >>= typeOfSymbol input)
            # Emit.write
                ( line at
                    ( b.name <> " = Puppy.Runtime.unbox (Puppy.Runtime.slot " <> show index <> " "
                        <> values
                        <> ")"
                    )
                )
        -- What an inlined rule would have computed, as a call rather than a
        -- nested `let`. A `let` would put the bindings out here in scope for
        -- the inlined rule's own code, which could then reach a name it never
        -- bound; a top-level function sees only what it was given.
        Core.FromAction _ ->
          spaced
            # Emit.write
                ( line at
                    ( b.name <> " = Puppy.Runtime.unbox (" <> path <> "_" <> show i <> " "
                        <> values
                        <> ")"
                    )
                )

  annotate at name = case _ of
    Nothing -> identity
    Just ty -> \acc ->
      Emit.write (spaces at <> name <> " :: ") acc
        # writeFragment at ty
        # Emit.write "\n"

  body at acc =
    let
      inner = if boxed then at + 2 else at

      opened =
        if boxed then
          Emit.write (line at "Puppy.Runtime.box") acc
            # Emit.write (spaces inner <> "(")
        else Emit.write (spaces inner <> "(") acc
    in
      case annotation of
        Just (Just ty) ->
          opened
            # Emit.write "("
            # writeFragment inner a.code
            # Emit.write ") :: "
            # writeFragment inner ty
            # Emit.write ")\n"
        _ ->
          opened
            # writeFragment inner a.code
            # Emit.write ")\n"

-- | Every action that was inlined into another, with the name it is called by.
-- |
-- | Names come from where the action sits rather than from a counter, so the
-- | pass that writes the calls and the pass that writes the definitions arrive
-- | at the same ones without having to agree on anything else.
inlineHelpers :: Input -> Array { name :: String, prod :: Prod, action :: Core.Action }
inlineHelpers input = Array.concatMap fromProduction
  (Array.mapWithIndex Tuple input.grammar.productions)
  where
  fromProduction (Tuple index prod) = case prod.source >>= sourceAction of
    Nothing -> []
    Just action -> collect prod (inlinePrefix index) action

  sourceAction i = Array.index input.core.productions i # map _.action

  collect prod path (Core.Action a) = Array.concatMap
    (nested prod path)
    (Array.mapWithIndex Tuple a.bindings)

  nested prod path (Tuple i b) = case b.value of
    Core.FromStack _ -> []
    Core.FromAction inner ->
      let
        name = path <> "_" <> show i
      in
        Array.cons { name, prod, action: inner } (collect prod name inner)

inlineHelperTable :: Input -> Emitter -> Emitter
inlineHelperTable input e = Array.foldl one e (inlineHelpers input)
  where
  one acc helper =
    Emit.write
      ( "\n" <> helper.name <> " :: Array Puppy.Runtime.Value -> Puppy.Runtime.Value\n"
          <> helper.name
          <> " "
          <> parameterFor helper.action
          <> " =\n"
      )
      acc
      # scoped input 2 helper.name helper.prod Nothing true helper.action

--------------------------------------------------------------------------------
-- The token type
--------------------------------------------------------------------------------

-- | Kept separate from the tables so that it can move to a module of its own
-- | when a lexer would rather not be recompiled every time a rule changes.
-- |
-- | Only the tokens the grammar declared appear here. End of input is not one
-- | of them: it is `Nothing` in the `Maybe Token` the driver actually reads, so
-- | a caller cannot write it at all -- which is stronger than leaving it out of
-- | the exports, and unlike that, possible. PureScript will not export some of
-- | a type's constructors and not others.
tokenType :: Input -> Emitter -> Emitter
tokenType input e
  | external input = e
tokenType input e =
  Array.foldl constructor (Emit.write "\ndata Token\n" e)
    (Array.mapWithIndex Tuple (tokenDecls input))
    # eqInstance
    # showInstance
  where
  derived name = Array.any (\d -> d.name == name) input.core.derives

  eqInstance acc =
    if derived "Eq" then Emit.write "\nderive instance Eq Token\n" acc
    else acc

  -- A `Show` that names the constructor and stops there asks nothing of the
  -- payload, so it can always be written. `%derive Show` asks for the payload
  -- as well, and takes on what that requires of its type.
  showInstance acc =
    Emit.write "\ninstance Show Token where\n  show = case _ of\n" acc
      # flip (Array.foldl (showArm (derived "Show"))) (tokenDecls input)

  showArm deep acc decl = Emit.write
    ( line 4
        ( if isJust decl.payload then
            if deep then
              decl.constructor <> " puppyPayload -> "
                <> quoted ("(" <> decl.constructor <> " ")
                <> " <> show puppyPayload <> "
                <> quoted ")"
            else decl.constructor <> " _ -> " <> quoted decl.constructor
          else decl.constructor <> " -> " <> quoted decl.constructor
        )
    )
    acc

  constructor acc (Tuple i decl) =
    let
      lead = if i == 0 then "  = " else "  | "
    in
      case decl.payload of
        Nothing -> Emit.write (lead <> decl.constructor <> "\n") acc
        Just ty ->
          Emit.write (lead <> decl.constructor <> " (") acc
            # writeFragment 4 ty
            # Emit.write ")\n"

-- | The terminals as the tables number them: everything declared, and then end
-- | of input. Only used for indices and names; the token type is built from the
-- | declared ones alone.
allTokens :: Input -> Array TokenDecl
allTokens input = Array.snoc (tokenDecls input)
  { name: eofToken.name
  , constructor: eofToken.constructor
  , display: eofToken.display
  , payload: Nothing
  , span: { start: nowhere, end: nowhere }
  }
  where
  nowhere = { offset: 0, line: 1, column: 1 }

terminalFunctions :: Input -> Emitter -> Emitter
terminalFunctions input e =
  terminalFunction "terminalIndex" "puppyIndexOf" "Int" "_" indexOf
    (show (Array.length tokens))
    indexNote
    e
    # terminalFunction "terminalValue" "puppyValueOf" "Puppy.Runtime.Value"
        "puppyPayload"
        valueOf
        ( "Puppy.Runtime.internalError\n" <> spaces 4
            <> quoted "a value was asked for a token this grammar does not declare"
        )
        valueNote
    # Emit.write "\nterminalNames :: Array String\n"
    # Emit.write (arrayBinding "terminalNames" (map (quoted <<< _.display) tokens))
    # Emit.write "\nterminalName :: Int -> String\nterminalName puppyIndex = case Puppy.Deps.index terminalNames puppyIndex of\n"
    # Emit.write (line 2 "Puppy.Deps.Just puppyFound -> puppyFound")
    # Emit.write (line 2 "Puppy.Deps.Nothing -> Puppy.Runtime.internalError")
    # Emit.write (line 4 ("(" <> quoted "no name for terminal " <> " <> show puppyIndex)"))
  where
  tokens = allTokens input

  numbered = Array.mapWithIndex Tuple tokens

  patterns = tokenPatterns input

  lastIndex = Array.length tokens - 1

  indexOf i _ = show i

  valueOf _ decl = "Puppy.Runtime.box "
    <> (if isJust decl.payload then "puppyPayload" else "unit")

  -- Where Puppy wrote the token type it knows the constructors, so a `case`
  -- over them is total and wants no fallback.
  --
  -- Where it did not, one is needed for a token the grammar never declared --
  -- and then the `Boolean` earns its keep. See the note it is emitted with.
  terminalFunction name helper result bound rhs fallback note acc
    | external input =
        Emit.write (helper <> " :: Boolean -> Puppy.Deps.Maybe ")
          (Emit.write note acc)
          # tokenRef input
          # Emit.write
              (" -> " <> result <> "\n" <> helper <> " = case _, _ of\n")
          # flip (Array.foldl (arm bound rhs "true, ")) numbered
          # Emit.write (line 2 ("_, _ -> " <> fallback))
          # Emit.write ("\n" <> name <> " :: Puppy.Deps.Maybe ")
          # tokenRef input
          # Emit.write
              (" -> " <> result <> "\n" <> name <> " = " <> helper <> " true\n")
    | otherwise =
        Emit.write ("\n" <> name <> " :: Puppy.Deps.Maybe ") acc
          # tokenRef input
          # Emit.write (" -> " <> result <> "\n" <> name <> " = case _ of\n")
          # flip (Array.foldl (arm bound rhs "")) numbered

  -- Why generated code classifies a token through a `Boolean` it always passes
  -- as `true`.
  --
  -- The patterns are the author's, and between them they may cover the token
  -- type -- a grammar and a token type written for one another usually do. A
  -- `case` whose last arm can be proved unreachable is an error under
  -- `--strict`, in generated code its reader did not write. Nothing covers
  -- `false`, so the last arm is always needed.
  --
  -- Pattern guards settle that too, and were tried. They settle too much: a
  -- guard is not something a compiler proves unreachable, so `T.Ident $$`
  -- written before `T.Ident "if"` would silently take every identifier and
  -- nothing would say so. This way the arms are still checked against one
  -- another.
  indexNote =
    "\n-- | Which terminal a token is.\n-- |\n"
      <> "-- | The `Boolean` is always `true`. It is there so that the last arm below\n"
      <> "-- | cannot be proved unreachable, whatever the patterns above it cover\n"
      <> "-- | between them -- while leaving those patterns checked against one\n"
      <> "-- | another, so that a pattern written where a narrower one should have come\n"
      <> "-- | first is still reported.\n"

  valueNote =
    "\n-- | The value a token carries, boxed for the parser stack. The `Boolean` is\n"
      <> "-- | there for the reason given above.\n"

  arm bound rhs prefix acc (Tuple i decl) =
    Emit.write (spaces 2 <> prefix) acc
      # match i decl bound
      # Emit.write (" -> " <> rhs i decl <> "\n")

  -- What stands to the left. In external mode this is the author's own
  -- pattern, written out with `bound` where the `$$` was; in generated mode
  -- Puppy knows the constructor because it wrote it.
  match i decl bound acc
    | i == lastIndex = Emit.write "Puppy.Deps.Nothing" acc
    | otherwise = case Array.index patterns i of
        Just pattern ->
          Emit.write "Puppy.Deps.Just (" acc
            # Emit.writePattern 2 bound pattern
            # Emit.write ")"
        Nothing -> Emit.write (declared decl bound) acc

  declared decl bound =
    if isJust decl.payload then
      "Puppy.Deps.Just (" <> decl.constructor <> " " <> bound <> ")"
    else "Puppy.Deps.Just " <> decl.constructor

-- | A multi-line array literal bound to a name, which is what these all want
-- | to be.
arrayBinding :: String -> Array String -> String
arrayBinding name entries = case Array.uncons entries of
  Nothing -> name <> " = []\n"
  Just { head, tail } ->
    name <> " =\n"
      <> line 2 ("[ " <> head)
      <> joinWith "" (map (line 2 <<< append ", ") tail)
      <> line 2 "]"

--------------------------------------------------------------------------------
-- The tables
--------------------------------------------------------------------------------

renderRuntimeAction :: Action -> String
renderRuntimeAction = case _ of
  Shift target -> "Puppy.Runtime.Shift " <> show target
  Reduce production -> "Puppy.Runtime.Reduce " <> show production
  Accept -> "Puppy.Runtime.Accept"

productionTable :: Input -> Emitter -> Emitter
productionTable input = Emit.write
  ( "\nproductionTable :: Array Puppy.Runtime.ProductionInfo\n"
      <> arrayBinding "productionTable" (map entry input.grammar.productions)
      <> "\nproductionAt :: Int -> Puppy.Runtime.ProductionInfo\nproductionAt puppyIndex = case Puppy.Deps.index productionTable puppyIndex of\n"
      <> line 2 "Puppy.Deps.Just puppyFound -> puppyFound"
      <> line 2 "Puppy.Deps.Nothing -> Puppy.Runtime.internalError"
      <> line 4 ("(" <> quoted "no production " <> " <> show puppyIndex)")
  )
  where
  entry p = "{ lhs: " <> show p.lhs <> ", arity: " <> show (Array.length p.rhs)
    <> ", name: "
    <> quoted (Core.renderProduction (asCore input p))
    <> " }"

-- | A production written the way a person reads it, for error messages.
asCore :: Input -> Prod -> Core.Production
asCore input p =
  { lhs: fromMaybe "?" (Array.index input.grammar.nonterminals p.lhs)
  , rhs: map named p.rhs
  , precedence: Nothing
  , action: Core.Action { bindings: [], code: { text: "", span: p.span } }
  , span: p.span
  }
  where
  named = case _ of
    T t -> Core.Terminal
      (fromMaybe "?" (map _.name (Array.index input.grammar.terminals t)))
    N n -> Core.Nonterminal
      (fromMaybe "?" (Array.index input.grammar.nonterminals n))

semanticActionTable :: Input -> Emitter -> Emitter
semanticActionTable input e =
  Emit.write
    "\nsemanticActionTable :: Array (Array Puppy.Runtime.Value -> Puppy.Runtime.Value)\nsemanticActionTable =\n"
    e
    # entries
    # Emit.write (line 2 "]")
    # Emit.write
        "\nsemanticActionAt :: Int -> Array Puppy.Runtime.Value -> Puppy.Runtime.Value\nsemanticActionAt puppyIndex = case Puppy.Deps.index semanticActionTable puppyIndex of\n"
    # Emit.write (line 2 "Puppy.Deps.Just puppyFound -> puppyFound")
    # Emit.write (line 2 "Puppy.Deps.Nothing -> Puppy.Runtime.internalError")
    # Emit.write (line 4 ("(" <> quoted "no semantic action " <> " <> show puppyIndex)"))
  where
  entries start = Array.foldl one start
    (Array.mapWithIndex Tuple input.grammar.productions)

  one acc (Tuple i prod) =
    Emit.write (spaces 2 <> (if i == 0 then "[ " else ", ")) acc
      # semanticAction input 6 i prod

sparseTable :: String -> String -> Array (Array String) -> String
sparseTable name entryType rows =
  "\n" <> name <> " :: Array (Array " <> entryType <> ")\n"
    <> arrayBinding name (map row rows)
  where
  row entries =
    if Array.null entries then "[]"
    else "[ " <> joinWith ", " entries <> " ]"

-- | Group a table's cells by the state they belong to, in one pass.
-- |
-- | Asking the map for each state's cells in turn walks the whole map once per
-- | state. An automaton is allowed tens of thousands of states, and a table has
-- | an entry for most of them, so that is quadratic in the size of the thing
-- | being written out.
rowsByState
  :: forall k v
   . Ord k
  => Int
  -> (k -> Int)
  -> (Tuple k v -> String)
  -> Map k v
  -> Array (Array String)
rowsByState count stateOf render cells =
  map (\state -> Array.reverse (fromMaybe [] (Map.lookup state grouped))) states
  where
  states = if count <= 0 then [] else Array.range 0 (count - 1)

  grouped = Array.foldl add Map.empty
    (Map.toUnfoldable cells :: Array (Tuple k v))

  add acc entry = Map.alter
    (Just <<< maybe [ render entry ] (Array.cons (render entry)))
    (stateOf (fst entry))
    acc

actionTable :: Input -> Emitter -> Emitter
actionTable input = Emit.write
  ( sparseTable "actionRows" "{ on :: Int, take :: Puppy.Runtime.Action }" rows
      <> "\nactionAt :: Int -> Int -> Puppy.Runtime.Action\nactionAt puppyState puppyTerminal = case Puppy.Deps.index actionRows puppyState of\n"
      <> line 2 "Puppy.Deps.Nothing -> Puppy.Runtime.internalError"
      <> line 4 ("(" <> quoted "no action row for state " <> " <> show puppyState)")
      <> line 2 "Puppy.Deps.Just puppyRow -> case Puppy.Deps.find (\\puppyEntry -> puppyEntry.on == puppyTerminal) puppyRow of"
      <> line 4 "Puppy.Deps.Just puppyEntry -> puppyEntry.take"
      <> line 4 "Puppy.Deps.Nothing -> Puppy.Runtime.Error"
  )
  where
  rows = rowsByState (Array.length input.automaton.states) _.state entry
    input.table.action

  entry (Tuple cell action) = "{ on: " <> show cell.terminal <> ", take: "
    <> renderRuntimeAction action
    <> " }"

gotoTable :: Input -> Emitter -> Emitter
gotoTable input = Emit.write
  ( sparseTable "gotoRows" "{ on :: Int, to :: Int }" rows
      <> "\ngotoAt :: Int -> Int -> Int\ngotoAt puppyState puppyNonterminal = case Puppy.Deps.index gotoRows puppyState of\n"
      <> line 2 "Puppy.Deps.Nothing -> Puppy.Runtime.internalError"
      <> line 4 ("(" <> quoted "no goto row for state " <> " <> show puppyState)")
      <> line 2 "Puppy.Deps.Just puppyRow -> case Puppy.Deps.find (\\puppyEntry -> puppyEntry.on == puppyNonterminal) puppyRow of"
      <> line 4 "Puppy.Deps.Just puppyEntry -> puppyEntry.to"
      <> line 4 "Puppy.Deps.Nothing -> Puppy.Runtime.internalError"
      <> line 6 ("(" <> quoted "no goto from state " <> " <> show puppyState <> " <> quoted " on " <> " <> show puppyNonterminal)")
  )
  where
  rows = rowsByState (Array.length input.automaton.states) _.state entry
    input.table.goto

  entry (Tuple cell target) = "{ on: " <> show cell.nonterminal <> ", to: "
    <> show target
    <> " }"

tableBuilder :: Input -> Emitter -> Emitter
tableBuilder input e =
  Emit.write "\ntableFor :: Int -> Puppy.Runtime.Table (Puppy.Deps.Maybe " e
    # tokenRef input
    # Emit.write ") Puppy.Runtime.Value\ntableFor puppyStart =\n"
    # Emit.write
        ( line 2 "{ action: actionAt"
            <> line 2 ", goto: gotoAt"
            <> line 2 ", production: productionAt"
            <> line 2 ", semanticAction: semanticActionAt"
            <> line 2 ", terminalIndex"
            <> line 2 ", terminalValue"
            <> line 2 ", terminalName"
            <> line 2
              (", terminalCount: " <> show (Array.length (allTokens input)))
            <> line 2 ", startState: puppyStart"
            <> line 2 "}"
        )
    # Emit.write
        ( "\n-- | End of input is `Puppy.Deps.Nothing` to the driver and nothing at all to a\n"
            <> "-- | caller, so an error that landed on it reports no token rather than one the\n"
            <> "-- | caller has no way to name.\ntoParseError\n"
            <> spaces 2
            <> ":: Puppy.Runtime.ParseError (Puppy.Deps.Maybe "
        )
    # tokenRef input
    # Emit.write (")\n" <> spaces 2 <> "-> Puppy.Runtime.ParseError ")
    # tokenRef input
    # Emit.write
        ( "\ntoParseError puppyError =\n"
            <> "  puppyError { found = Puppy.Deps.fromMaybe Puppy.Deps.Nothing puppyError.found }\n"
            <> "\nfromResult\n"
            <> line 2 ":: forall puppyResult"
            <> spaces 3
            <> ". Puppy.Deps.Either (Puppy.Runtime.ParseError (Puppy.Deps.Maybe "
        )
    # tokenRef input
    # Emit.write
        ( ")) Puppy.Runtime.Value\n"
            <> spaces 2
            <> "-> Puppy.Deps.Either (Puppy.Runtime.ParseError "
        )
    # tokenRef input
    # Emit.write
        ( ") puppyResult\nfromResult = case _ of\n"
            <> line 2
              "Puppy.Deps.Left puppyError -> Puppy.Deps.Left (toParseError puppyError)"
            <> line 2
              "Puppy.Deps.Right puppyValue -> Puppy.Deps.Right (Puppy.Runtime.unbox puppyValue)"
            <> runArrayNote
            <> spaces 2
            <> ":: Array "
        )
    # tokenRef input
    # Emit.write
        ( "\n"
            <> line 2 "-> Int"
            <> spaces 2
            <> "-> Puppy.Runtime.Step (Puppy.Deps.Maybe "
        )
    # tokenRef input
    # Emit.write
        ( ") Puppy.Runtime.Value\n"
            <> spaces 2
            <> "-> Puppy.Deps.Either (Puppy.Runtime.ParseError (Puppy.Deps.Maybe "
        )
    # tokenRef input
    # Emit.write
        ( ")) Puppy.Runtime.Value\n"
            <> "runArray puppyInput puppyIndex puppyStep = case puppyStep of\n"
            <> line 2 "Puppy.Runtime.Await puppyResume ->"
            <> line 4 "runArray puppyInput (puppyIndex + 1)"
            <> line 6
              "(Puppy.Runtime.resume puppyResume (Puppy.Deps.index puppyInput puppyIndex))"
            <> line 2 "Puppy.Runtime.Done puppyValue -> Puppy.Deps.Right puppyValue"
            <> line 2 "Puppy.Runtime.Failed puppyError -> Puppy.Deps.Left puppyError"
        )
  where
  -- The same point either way; it can only be made by naming the token type
  -- where there is a name for it.
  runArrayNote
    | external input =
        "\n-- | Feed the parser from an array, without building a second one.\n"
          <> "-- |\n"
          <> "-- | The driver reads a `Puppy.Deps.Maybe` of the token type, and that is\n"
          <> "-- | exactly what a lookup past the end of an array answers, so the same\n"
          <> "-- | `Puppy.Deps.index` both reads a token and says there are no more.\nrunArray\n"
    | otherwise =
        "\n-- | Feed the parser from an array, without building a second one.\n"
          <> "-- |\n"
          <> "-- | The driver's token here is `Puppy.Deps.Maybe Token`, and that is exactly\n"
          <> "-- | what a lookup past the end of an array answers, so the same\n"
          <> "-- | `Puppy.Deps.index` both reads a token and says there are no more.\nrunArray\n"

-- | The two ways in, for each `%start`.
-- |
-- | One takes the tokens all at once and one asks for them as it needs them.
-- | They share a table and a state, and differ only in where the next token
-- | comes from, which is the whole point of the driver being a machine that
-- | stops rather than a loop over an array.
entryPoints :: Input -> Emitter -> Emitter
entryPoints input e = Array.foldl one e
  (Array.zipWith Tuple input.grammar.starts input.automaton.initial)
  where
  -- Parenthesised because it lands in an argument position: a bare
  -- `Array String` would be read as two arguments.
  resultType start acc = case Map.lookup start.name (declaredTypes input) of
    Just ty -> Emit.write "(" acc # writeFragment 2 ty # Emit.write ")"
    Nothing -> Emit.write "Puppy.Runtime.Value" acc

  one acc (Tuple start state) =
    Emit.write ("\n" <> start.name <> " :: Array ") acc
      # tokenRef input
      # Emit.write " -> Puppy.Deps.Either (Puppy.Runtime.ParseError "
      # tokenRef input
      # Emit.write ") "
      # resultType start
      # Emit.write "\n"
      # Emit.write
          ( start.name <> " puppyInput =\n"
              <> line 2
                ( "fromResult (runArray puppyInput 0 (Puppy.Runtime.start (tableFor "
                    <> show state
                    <> ")))"
                )
          )
      # Emit.write
          ( "\n" <> Names.streamingName start.name <> "\n"
              <> line 2 ":: forall m"
              <> line 3 ". Puppy.Deps.MonadRec m"
              <> spaces 2
              <> "=> m (Puppy.Deps.Maybe "
          )
      # tokenRef input
      # Emit.write
          ( ")\n" <> spaces 2
              <> "-> m (Puppy.Deps.Either (Puppy.Runtime.ParseError "
          )
      # tokenRef input
      # Emit.write ") "
      # resultType start
      # Emit.write ")\n"
      # Emit.write
          ( Names.streamingName start.name <> " puppyNext =\n"
              <> line 2
                ( "map fromResult (Puppy.Runtime.parseM (tableFor "
                    <> show state
                    <> ") puppyNext)"
                )
          )

-- | What the module exports.
-- |
-- | The token type is there only when Puppy wrote it. An external one belongs
-- | to the module that declared it, and re-exporting someone else's type under
-- | this module's name would give a reader two ways to say the same thing and
-- | no reason to prefer either.
exportList :: Input -> String
exportList input = case Array.uncons exported of
  -- A grammar with no start symbol never reaches the generator.
  Nothing -> ""
  Just { head, tail } ->
    line 2 ("( " <> head)
      <> joinWith "" (map (\name -> line 2 (", " <> name)) tail)
  where
  exported =
    (if external input then [] else [ "Token(..)" ])
      <> Array.concatMap
        (\s -> [ s.name, Names.streamingName s.name ])
        input.grammar.starts

preamble :: Input -> Emitter -> Emitter
preamble input e =
  Emit.write
    ( "-- Generated by Puppy. Do not edit.\n--\n"
        <> "-- End of input is not a token. It is `Puppy.Deps.Nothing`: an entry point\n"
        <> "-- taking an array reaches it by running off the end, and one pulling its\n"
        <> "-- tokens by asking for another and being told there are none. Either way no\n"
        <> "-- caller can write one in the middle of the input and have the rest of it\n"
        <> "-- silently ignored.\n"
        <> "module "
        <> input.moduleName
        <> "\n"
        <> exportList input
        <> line 2 ") where"
        <> "\nimport Prelude\n\n"
        <> "import Puppy.Runtime as Puppy.Runtime\n"
        <> "import Puppy.Runtime.Deps as Puppy.Deps\n"
    )
    e
    # header
  where
  header acc = case input.core.header of
    Nothing -> acc
    Just code -> Emit.write "\n" acc # writeFragment 0 code # Emit.write "\n"

generate :: Input -> Either String Generated
generate input =
  if not (Names.isModuleName input.moduleName) then
    Left
      ( "`" <> input.moduleName
          <> "` is not a PureScript module name; it has to be dot-separated words, each beginning with a capital letter"
      )
  else if Array.length input.automaton.initial /= Array.length input.grammar.starts then
    Left
      ( "internal error: the automaton has "
          <> show (Array.length input.automaton.initial)
          <> " entry states but the grammar has "
          <> show (Array.length input.grammar.starts)
          <> " start symbols"
      )
  else
    let
      e = Emit.empty
        # preamble input
        # tokenType input
        # terminalFunctions input
        # productionTable input
        # semanticActionTable input
        # inlineHelperTable input
        # actionTable input
        # gotoTable input
        # tableBuilder input
        # entryPoints input
    in
      Right { source: Emit.render e, mappings: Emit.mappings e }
