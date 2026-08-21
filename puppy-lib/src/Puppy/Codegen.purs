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
tokenDecls input = input.core.terminals

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
      ( "\\_ -> Runtime.internalError\n"
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
                    ( b.name <> " = Runtime.unbox (Runtime.slot " <> show index <> " "
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
                    ( b.name <> " = Runtime.unbox (" <> path <> "_" <> show i <> " "
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
          Emit.write (line at "Runtime.box") acc
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
      ( "\n" <> helper.name <> " :: Array Runtime.Value -> Runtime.Value\n"
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
tokenType input e =
  Array.foldl constructor (Emit.write "data Token\n" e)
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
  Emit.write "\nterminalIndex :: Maybe.Maybe Token -> Int\nterminalIndex = case _ of\n" e
    # flip (Array.foldl indexArm) numbered
    # Emit.write "\nterminalValue :: Maybe.Maybe Token -> Runtime.Value\nterminalValue = case _ of\n"
    # flip (Array.foldl valueArm) numbered
    # Emit.write "\nterminalNames :: Array String\n"
    # Emit.write (arrayBinding "terminalNames" (map (quoted <<< _.display) tokens))
    # Emit.write "\nterminalName :: Int -> String\nterminalName puppyIndex = case Array.index terminalNames puppyIndex of\n"
    # Emit.write (line 2 "Maybe.Just puppyFound -> puppyFound")
    # Emit.write (line 2 "Maybe.Nothing -> Runtime.internalError")
    # Emit.write (line 4 ("(" <> quoted "no name for terminal " <> " <> show puppyIndex)"))
  where
  tokens = allTokens input

  numbered = Array.mapWithIndex Tuple tokens

  -- The index does not look at a payload, so it must not name it: an unused
  -- name is a warning, and a caller building with `--strict` would have that
  -- warning fail their build rather than ours.
  indexArm acc (Tuple i decl) =
    Emit.write (line 2 (pattern i decl "_" <> " -> " <> show i)) acc

  valueArm acc (Tuple i decl) = Emit.write
    ( line 2
        ( pattern i decl "puppyPayload" <> " -> Runtime.box "
            <> (if isJust decl.payload then "puppyPayload" else "unit")
        )
    )
    acc

  pattern i decl bound =
    if i == Array.length tokens - 1 then "Maybe.Nothing"
    else if isJust decl.payload then
      "Maybe.Just (" <> decl.constructor <> " " <> bound <> ")"
    else "Maybe.Just " <> decl.constructor

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
  Shift target -> "Runtime.Shift " <> show target
  Reduce production -> "Runtime.Reduce " <> show production
  Accept -> "Runtime.Accept"

productionTable :: Input -> Emitter -> Emitter
productionTable input = Emit.write
  ( "\nproductionTable :: Array Runtime.ProductionInfo\n"
      <> arrayBinding "productionTable" (map entry input.grammar.productions)
      <> "\nproductionAt :: Int -> Runtime.ProductionInfo\nproductionAt puppyIndex = case Array.index productionTable puppyIndex of\n"
      <> line 2 "Maybe.Just puppyFound -> puppyFound"
      <> line 2 "Maybe.Nothing -> Runtime.internalError"
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
    "\nsemanticActionTable :: Array (Array Runtime.Value -> Runtime.Value)\nsemanticActionTable =\n"
    e
    # entries
    # Emit.write (line 2 "]")
    # Emit.write
        "\nsemanticActionAt :: Int -> Array Runtime.Value -> Runtime.Value\nsemanticActionAt puppyIndex = case Array.index semanticActionTable puppyIndex of\n"
    # Emit.write (line 2 "Maybe.Just puppyFound -> puppyFound")
    # Emit.write (line 2 "Maybe.Nothing -> Runtime.internalError")
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
  ( sparseTable "actionRows" "{ on :: Int, take :: Runtime.Action }" rows
      <> "\nactionAt :: Int -> Int -> Runtime.Action\nactionAt puppyState puppyTerminal = case Array.index actionRows puppyState of\n"
      <> line 2 "Maybe.Nothing -> Runtime.internalError"
      <> line 4 ("(" <> quoted "no action row for state " <> " <> show puppyState)")
      <> line 2 "Maybe.Just puppyRow -> case Array.find (\\puppyEntry -> puppyEntry.on == puppyTerminal) puppyRow of"
      <> line 4 "Maybe.Just puppyEntry -> puppyEntry.take"
      <> line 4 "Maybe.Nothing -> Runtime.Error"
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
      <> "\ngotoAt :: Int -> Int -> Int\ngotoAt puppyState puppyNonterminal = case Array.index gotoRows puppyState of\n"
      <> line 2 "Maybe.Nothing -> Runtime.internalError"
      <> line 4 ("(" <> quoted "no goto row for state " <> " <> show puppyState)")
      <> line 2 "Maybe.Just puppyRow -> case Array.find (\\puppyEntry -> puppyEntry.on == puppyNonterminal) puppyRow of"
      <> line 4 "Maybe.Just puppyEntry -> puppyEntry.to"
      <> line 4 "Maybe.Nothing -> Runtime.internalError"
      <> line 6 ("(" <> quoted "no goto from state " <> " <> show puppyState <> " <> quoted " on " <> " <> show puppyNonterminal)")
  )
  where
  rows = rowsByState (Array.length input.automaton.states) _.state entry
    input.table.goto

  entry (Tuple cell target) = "{ on: " <> show cell.nonterminal <> ", to: "
    <> show target
    <> " }"

tableBuilder :: Input -> Emitter -> Emitter
tableBuilder input = Emit.write
  ( "\ntableFor :: Int -> Runtime.Table (Maybe.Maybe Token) Runtime.Value\ntableFor puppyStart =\n"
      <> line 2 "{ action: actionAt"
      <> line 2 ", goto: gotoAt"
      <> line 2 ", production: productionAt"
      <> line 2 ", semanticAction: semanticActionAt"
      <> line 2 ", terminalIndex"
      <> line 2 ", terminalValue"
      <> line 2 ", terminalName"
      <> line 2 (", terminalCount: " <> show (Array.length (allTokens input)))
      <> line 2 ", startState: puppyStart"
      <> line 2 "}"
      <> "\nfeed :: Array Token -> Array (Maybe.Maybe Token)\nfeed puppyInput = Array.snoc (map Maybe.Just puppyInput) Maybe.Nothing\n"
      <> "\n-- | End of input is `Maybe.Nothing` to the driver and nothing at all to a\n-- | caller, so an error that landed on it reports no token rather than one the\n-- | caller has no way to name.\ntoParseError\n"
      <> line 2 ":: Runtime.ParseError (Maybe.Maybe Token)"
      <> line 2 "-> Runtime.ParseError Token"
      <> "toParseError puppyError =\n  puppyError { found = Maybe.fromMaybe Maybe.Nothing puppyError.found }\n"
  )

entryPoints :: Input -> Emitter -> Emitter
entryPoints input e = Array.foldl one e
  (Array.zipWith Tuple input.grammar.starts input.automaton.initial)
  where
  one acc (Tuple start state) =
    let
      result = Map.lookup start.name (declaredTypes input)
    in
      Emit.write ("\n" <> start.name <> " :: Array Token -> Either.Either (Runtime.ParseError Token) ") acc
        #
          ( case result of
              -- Parenthesised because it lands in an argument position: a bare
              -- `Array String` would be read as two arguments.
              Just ty -> \out ->
                Emit.write "(" out # writeFragment 2 ty # Emit.write ")"
              Nothing -> Emit.write "Runtime.Value"
          )
        # Emit.write "\n"
        # Emit.write
            ( start.name <> " puppyInput =\n"
                <> line 2
                  ( "case Runtime.parse (tableFor " <> show state
                      <> ") (feed puppyInput) of"
                  )
                <> line 4
                  "Either.Left puppyError -> Either.Left (toParseError puppyError)"
                <> line 4
                  "Either.Right puppyValue -> Either.Right (Runtime.unbox puppyValue)"
            )

preamble :: Input -> Emitter -> Emitter
preamble input e =
  Emit.write
    ( "-- Generated by Puppy. Do not edit.\n--\n"
        <> "-- End of input is not a token. It is the `Maybe.Nothing` the entry points\n"
        <> "-- below append, which is why no caller can write one in the middle of the\n"
        <> "-- input and have the rest of it silently ignored.\n"
        <> "module "
        <> input.moduleName
        <> "\n"
        <> line 2 "( Token(..)"
        <> joinWith "" (map (\s -> line 2 (", " <> s.name)) input.grammar.starts)
        <> line 2 ") where"
        <> "\nimport Prelude\n\n"
        <> "import Data.Array as Array\n"
        <> "import Data.Either as Either\n"
        <> "import Data.Maybe as Maybe\n"
        <> "import Puppy.Runtime as Runtime\n"
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
        # Emit.write "\n"
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
