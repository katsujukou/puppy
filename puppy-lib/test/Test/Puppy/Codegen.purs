module Test.Puppy.Codegen (spec) where

import Prelude

import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith, replaceAll, split)
import Data.String.Pattern (Replacement(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.String.Pattern (Pattern(..)) as P
import Effect.Aff (Aff)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Puppy.Codegen (Input, generate)
import Puppy.Syntax as Syntax
import Puppy.Codegen.Emit (SourceMapping)
import Puppy.Expand (expand)
import Puppy.LR.Analysis (analyse)
import Puppy.LR.Automaton (Policy(..), build)
import Puppy.LR.Grammar (number)
import Puppy.LR.Table (tabulate)
import Puppy.Syntax.Parser as Parser
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

grammar :: String
grammar =
  """
%{
import Data.Maybe (Maybe(..))
%}

%token PLUS
%token { Int } INT

%start { Int } total

%type { Int } expr

%left PLUS

%%

total:
  | e = expr { e }

expr:
  | i = INT                { i }
  | a = expr PLUS b = expr
      { let doubled = a + a
        in doubled + b - a
      }
"""

-- | A grammar whose alternatives share a prefix, so the automaton grows with
-- | `n` and so does every table written for it.
wide :: Int -> String
wide n =
  "%token A P C D\n%start { Int } main\n%%\n"
    <> "main: | C x = u { 0 } | D x = u { 0 }\nu:\n"
    <> joinWith "" (map (\i -> "  | A t" <> show i <> " { 0 }\n") (Array.range 1 n))
    <> joinWith "" (map (\i -> "t" <> show i <> ": | P { 0 }\n") (Array.range 1 n))

prepared :: Syntax.Grammar -> Either String Input
prepared syn = case expand syn of
  Left errs -> Left (joinWith "; " (map _.message errs))
  Right core -> case number core of
    Left errs -> Left (joinWith "; " (map _.message errs))
    Right g -> case build Pager g (analyse g) of
      Left errs -> Left (joinWith "; " (map _.message errs))
      Right automaton -> Right
        { moduleName: "Fixture.Parser"
        , core
        , grammar: g
        , automaton
        , table: tabulate g automaton
        }

codes :: Array Int
codes = [ 0, 15, 16, 31, 127 ]

lines :: String -> Array String
lines = split (P.Pattern "\n")

-- | The `terminalNames` entry a display name turns into.
-- |
-- | The grammar is built by hand rather than with `show`, because the point is
-- | to put the character itself into the display name and see what comes out
-- | the other end.
quotedDisplay :: String -> Either String String
quotedDisplay display = map entry (generated source)
  where
  source = "%token A \"" <> display
    <> "\"\n%start { Int } t\n%%\nt: | A { 0 }\n"

  entry out = case Array.findIndex (_ == "terminalNames =") (lines out.source) of
    Nothing -> ""
    Just i -> case Array.index (lines out.source) (i + 1) of
      Nothing -> ""
      Just l -> SCU.drop 4 l

withDerive :: String -> String
withDerive = replaceAll (P.Pattern "%start") (Replacement "%derive Eq Show\n%start")

generated :: String -> Either String { source :: String, mappings :: Array SourceMapping }
generated = generatedIn "Fixture.Parser"

generatedIn
  :: String
  -> String
  -> Either String { source :: String, mappings :: Array SourceMapping }
generatedIn moduleName source = case Parser.parse source of
  Left err -> Left ("parse: " <> err.message)
  Right syn -> case expand syn of
    Left errs -> Left (joinWith "; " (map _.message errs))
    Right core -> case number core of
      Left errs -> Left (joinWith "; " (map _.message errs))
      Right g -> case build Pager g (analyse g) of
        Left errs -> Left (joinWith "; " (map _.message errs))
        Right automaton -> generate
          { moduleName
          , core
          , grammar: g
          , automaton
          , table: tabulate g automaton
          }

-- | The same, over a token type the grammar only refers to.
-- |
-- | Two of these patterns are the interesting shapes for a mapping: one with
-- | code after the hole on the same line, and one spread over several lines
-- | with the hole part way down.
externalGrammar :: String
externalGrammar =
  """
%{
import Language.Token as T
%}

%tokentype { T.Token }

%token PLUS @ { T.At _ T.Plus }
%token { Int } INT "an integer" { T.At _ (T.Number $$) }
%token { String } NAME "a name"
  { T.At _
      (T.Ident $$)
  }

%start { Int } total

%%

total:
  | i = INT                  { i }
  | n = NAME                 { 0 }
  | a = total PLUS b = INT   { a + b }
"""

withGenerated
  :: (String -> Array SourceMapping -> Aff Unit) -> Aff Unit
withGenerated k = case generated grammar of
  Left message -> fail ("failed to generate: " <> message)
  Right out -> k out.source out.mappings

-- | The text of one line of the generated module.
generatedLine :: String -> Int -> String
generatedLine source n =
  case Array.index (split (P.Pattern "\n") source) (n - 1) of
    Nothing -> ""
    Just l -> l

startsWith :: String -> String -> Boolean
startsWith prefix text = SCU.take (SCU.length prefix) text == prefix

-- | Every line of a fragment, checked where the mapping says it is.
-- |
-- | The first line begins at the recorded column. Every line after it begins at
-- | the recorded indent, with whatever leading space it had in the grammar
-- | still in front of it -- which is what makes the layout survive the move.
-- |
-- | Only a verbatim mapping says the generated text is the source text. What
-- | Puppy wrote in place of a `$$` says the opposite, and is checked by
-- | `uncoveredHoles` instead.
misplacedLines :: String -> String -> SourceMapping -> Array String
misplacedLines grammar source m
  | not m.verbatim = []
misplacedLines grammar source m =
  Array.mapMaybe check (Array.mapWithIndex Tuple fragment)
  where
  fragment = split (P.Pattern "\n")
    ( SCU.take (m.source.end.offset - m.source.start.offset)
        (SCU.drop m.source.start.offset grammar)
    )

  check (Tuple k expected) =
    let
      column = if k == 0 then m.generated.start.column else m.indent + 1

      found = SCU.drop (column - 1) (generatedLine source (m.generated.start.line + k))
    in
      if startsWith expected found then Nothing
      else Just
        ( "line " <> show (k + 1) <> " of a fragment: expected " <> show expected
            <> " at line "
            <> show (m.generated.start.line + k)
            <> ", column "
            <> show column
            <> ", found "
            <> show found
        )

-- | Where every `$$` in a grammar is.
-- |
-- | A plain search, which is only right because these grammars are written not
-- | to need anything cleverer: no `$$` of theirs hides in a string or a
-- | comment. Telling those apart is the lexer's job and is tested there.
placeholderOffsets :: String -> Array Int
placeholderOffsets text = Array.filter isHole
  (Array.range 0 (SCU.length text - 2))
  where
  isHole i = SCU.take 2 (SCU.drop i text) == "$$"

-- | The line the fragment should have ended on.
badEnd :: String -> SourceMapping -> Boolean
badEnd grammar m = m.generated.end.line /= m.generated.start.line + newlines
  where
  newlines =
    Array.length
      ( split (P.Pattern "\n")
          ( SCU.take (m.source.end.offset - m.source.start.offset)
              (SCU.drop m.source.start.offset grammar)
          )
      ) - 1

spec :: Spec Unit
spec = describe "Puppy.Codegen" do
  it "writes a module that says what the grammar said" do
    withGenerated \source _ -> do
      mentions source "module Fixture.Parser"
      mentions source "data Token"
      mentions source "INT (Int)"
      -- Verbatim, from the `%{ ... %}` block.
      mentions source "import Data.Maybe (Maybe(..))"
      mentions source "total :: Array Token -> Puppy.Deps.Either"

  -- Every line of the author's own code has to be findable again, not just the
  -- first: a compiler complaining about the second line of an action is the
  -- case this exists for.
  it "points every line of every fragment at the text it came from" do
    withGenerated \source ms -> do
      (Array.length ms > 0) `shouldEqual` true
      case Array.head (Array.concatMap (misplacedLines grammar source) ms) of
        Nothing -> pure unit
        Just problem -> fail problem

  it "records where each fragment ends, not only where it starts" do
    withGenerated \_ ms ->
      Array.length (Array.filter (badEnd grammar) ms) `shouldEqual` 0

  -- A pattern comes out with something else where its `$$` was, so everything
  -- after the hole sits at a different column from the one it was written at.
  -- Each stretch around a hole is recorded on its own; one mapping for the
  -- whole pattern would point ten columns wide of the mark.
  it "points at every stretch of a token pattern, on both sides of a hole" do
    case generated externalGrammar of
      Left message -> fail ("failed to generate: " <> message)
      Right out -> do
        (Array.length out.mappings > 0) `shouldEqual` true
        case
          Array.head
            ( Array.concatMap (misplacedLines externalGrammar out.source)
                out.mappings
            )
          of
          Nothing -> pure unit
          Just problem -> fail problem
        Array.length (Array.filter (badEnd externalGrammar) out.mappings)
          `shouldEqual` 0

  -- A compiler complaining about the `puppyPayload` Puppy put in has to have
  -- somewhere to point, and the stretches either side of a hole cover the
  -- author's text rather than the hole. Each placeholder is written twice --
  -- once into `terminalIndex` and once into `terminalValue` -- so each should
  -- be claimed twice.
  it "covers each placeholder with a mapping of its own" do
    case generated externalGrammar of
      Left message -> fail ("failed to generate: " <> message)
      Right out -> do
        let
          holes = placeholderOffsets externalGrammar
          claimed = map (_.source.start.offset)
            (Array.filter (not <<< _.verbatim) out.mappings)
        (Array.length holes > 0) `shouldEqual` true
        Array.sort claimed `shouldEqual` Array.sort (holes <> holes)

  -- The patterns have to end up in one `case`, because that is the only thing
  -- that checks them against one another: a pattern the ones above it already
  -- cover is an unreachable arm, and the compiler says so. Pattern guards would
  -- classify the tokens just as well and say nothing, which is why the
  -- generated code carries a discriminant it always passes as `true` -- that is
  -- what keeps the last arm from being provably dead without switching to
  -- guards.
  it "puts the patterns in one case, so the compiler still checks them" do
    case generated externalGrammar of
      Left message -> fail ("failed to generate: " <> message)
      Right out -> do
        mentions out.source "puppyIndexOf :: Boolean -> Puppy.Deps.Maybe"
        mentions out.source "puppyIndexOf = case _, _ of"
        mentions out.source "terminalIndex = puppyIndexOf true"
        mentions out.source "puppyValueOf = case _, _ of"
        mentions out.source "terminalValue = puppyValueOf true"
        notMentions out.source "<- puppyToken"

  it "writes the author's own type where the token type goes" do
    case generated externalGrammar of
      Left message -> fail ("failed to generate: " <> message)
      Right out -> do
        mentions out.source "total :: Array (T.Token)"
        mentions out.source "Puppy.Deps.Just (T.At _ (T.Number puppyPayload))"
        mentions out.source "Puppy.Deps.Just (T.At _ (T.Number _))"
        -- Nothing of Puppy's own is declared for it, and nothing is exported.
        notMentions out.source "data Token"
        notMentions out.source "Token(..)"

  -- `@` says the value is the token itself, so the arm that answers with a
  -- value needs a name for what it matched and the one that answers with a
  -- number does not.
  it "names the token where a terminal carries the whole of it" do
    case generated externalGrammar of
      Left message -> fail ("failed to generate: " <> message)
      Right out -> do
        mentions out.source
          "Just (puppyToken@(T.At _ T.Plus)) -> Puppy.Runtime.box puppyToken"
        mentions out.source "Just (T.At _ T.Plus) -> 0"
        notMentions out.source "puppyToken@(T.At _ T.Plus)) -> 0"

  it "escapes what cannot be written into a string literal" do
    case generated "%token A \"two\nlines\"\n%start { Int } t\n%%\nt: | A { 0 }\n" of
      Left message -> fail ("failed to generate: " <> message)
      Right out -> do
        mentions out.source "\"two\\nlines\""
        when (contains (Pattern "\"two\nlines\"") out.source) do
          fail "a real newline was written into a string literal"

  it "refuses a module name that is not one" do
    case generatedIn "not a module" grammar of
      Right _ -> fail "expected a rejection"
      Left message ->
        when (not (contains (Pattern "module name") message)) do
          fail ("unexpected message: " <> message)

  -- A token can carry anything, including something with no `Eq` at all, so
  -- neither instance is assumed.
  describe "instances on the token type" do
    it "leaves out what was not asked for" do
      withGenerated \source _ -> do
        when (contains (Pattern "derive instance Eq Token") source) do
          fail "Eq was derived without being asked for"
        mentions source "INT _ -> \"INT\""

    it "writes what %derive asks for" do
      case generated (withDerive grammar) of
        Left message -> fail ("failed to generate: " <> message)
        Right out -> do
          mentions out.source "derive instance Eq Token"
          mentions out.source
            "INT puppyPayload -> \"(INT \" <> show puppyPayload <> \")\""

  -- Asking the tables for one state's cells at a time walks the whole table
  -- once per state, and an automaton is allowed tens of thousands of them.
  -- Timed by hand: this is pure and synchronous, so the spec timeout cannot
  -- fire while it runs.
  it "writes a large table without walking it once per state" do
    case Parser.parse (wide 1500) of
      Left err -> fail ("failed to parse: " <> err.message)
      Right syn -> case prepared syn of
        Left message -> fail message
        Right ready -> do
          Milliseconds start <- liftEffect (unInstant <$> now)
          let out = generate ready
          Milliseconds end <- liftEffect (unInstant <$> now)
          case out of
            Left message -> fail message
            Right written -> do
              (SCU.length written.source > 0) `shouldEqual` true
              let elapsed = end - start
              when (elapsed >= 2000.0) do
                fail
                  ( "writing a table for " <> show (Array.length ready.automaton.states)
                      <> " states took "
                      <> show elapsed
                      <> "ms"
                  )

  -- `\x` is greedy and PureScript has no `\&`, so the boundaries matter more
  -- than the digits do.
  describe "escaping control characters" do
    it "pads every escape to two digits" do
      traverse (quotedDisplay <<< SCU.singleton)
        (Array.mapMaybe fromCharCode codes)
        `shouldEqual` Right
          [ "\"\\x00\"", "\"\\x0f\"", "\"\\x10\"", "\"\\x1f\"", "\"\\x7f\"" ]

    it "closes the literal when a hex digit follows an escape" do
      -- Spelled the long way for the same reason the generator has to: written
      -- as one literal, the `A` would join the escape.
      quotedDisplay ("\x01" <> "A") `shouldEqual` Right "\"\\x01\" <> \"A\""

    it "leaves a named escape alone when a hex digit follows" do
      quotedDisplay "\tb" `shouldEqual` Right "\"\\tb\""
  where
  mentions source needle =
    when (not (contains (Pattern needle) source)) do
      fail ("expected the generated module to mention " <> show needle)

  notMentions source needle =
    when (contains (Pattern needle) source) do
      fail ("expected the generated module not to mention " <> show needle)
