-- | The generated parser and the hand-written one, over the same file.
-- |
-- | Both are given source text and both lex it with
-- | `purescript-language-cst-parser`'s lexer, so what differs between the two
-- | numbers is the parsing and the tree. The lexer is timed on its own as well,
-- | because it is in both of the others.
-- |
-- | The two are not doing the same amount of work, and that is worth stating
-- | before the numbers are read. `PureScript.CST.parseModule` builds a tree
-- | that keeps every token it read, comments and all, because a formatter has
-- | to put the source back. The tree here keeps names, shapes and positions,
-- | and drops the punctuation.
-- |
-- | ```sh
-- | spago test -p puppy-example-purescript -m Test.Puppy.Purs.Bench --pure -- FILE
-- | ```
module Test.Puppy.Purs.Bench where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), isRight)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number.Format as Format
import Data.String.CodeUnits as SCU
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Process as Process
import Puppy.Purs.Run as Run
import Puppy.Runtime (RecoveryResult(..))
import PureScript.CST as CST
import PureScript.CST.Lexer as Lexer
import PureScript.CST.TokenStream (TokenStep(..))
import PureScript.CST.TokenStream as TokenStream

-- | How long one sample should take, in milliseconds.
-- |
-- | Not a number of repetitions: a time to reach. The clock here is
-- | `Date.now`, which counts whole milliseconds, so a single parse of a small
-- | file is a handful of ticks and the difference between two of them is
-- | mostly rounding. A sample is therefore a run of repetitions long enough to
-- | be worth timing, divided back down afterwards.
-- |
-- | How many repetitions that is comes from `calibrate`, which measures rather
-- | than estimates: it runs the work, and if the run was short it asks for
-- | more and runs it again. That happens twice, so the count is settled on
-- | code the engine has already warmed rather than on a cold run -- a cold run
-- | says the work costs more than it does, and asks for too few repetitions to
-- | reach this figure once it is warm. `calibrate` aims a little over it for
-- | the same reason, and each sample's length is reported so that the floor
-- | can be seen to have held.
sampleMillis :: Number
sampleMillis = 200.0

rounds :: Int
rounds = 10

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  case Array.head args of
    Nothing -> die "a file to parse is required"
    Just path -> do
      source <- FS.readTextFile UTF8 path

      -- Nothing is timed until both sides are known to be reading the same
      -- thing. A parser that rejects the input would otherwise be timed on how
      -- quickly it gives up, and come out ahead for it.
      unless (lexes source) (die (path <> ": the lexer does not read it"))
      unless (puppyParses source) (die (path <> ": puppy does not parse it"))
      unless (puppyRecovers source) (die (path <> ": puppy does not parse it cleanly"))
      unless (cstParses source) (die (path <> ": language-cst-parser does not parse it"))

      Console.log (path <> ": " <> show (SCU.length source) <> " characters")
      time "lex only           " \_ -> lexes source
      time "puppy              " \_ -> puppyParses source
      time "puppy (recovering) " \_ -> puppyRecovers source
      time "language-cst-parser" \_ -> cstParses source
  where
  die message = do
    Console.error message
    Process.exit' 1

  lexes source = walk (Lexer.lexModule source)

  walk stream = case TokenStream.step stream of
    TokenEOF _ _ -> true
    TokenError _ _ _ _ -> false
    TokenCons _ _ rest _ -> walk rest

  puppyParses source = case Run.parseModule source of
    Right inner -> isRight inner
    Left _ -> false

  -- The recovering entry point over a file with nothing wrong with it, which
  -- is what prices carrying a recovery path for a parse that never needs one.
  -- Only a clean answer counts, for the same reason it does below.
  puppyRecovers source = case Run.parseModuleRecovering source of
    Right (ParseSucceeded _) -> true
    _ -> false

  -- Only a clean parse counts. `ParseSucceededWithErrors` means the recovery
  -- path ran, which is neither the same work nor the same answer.
  cstParses source = case CST.parseModule source of
    CST.ParseSucceeded _ -> true
    _ -> false

  time name work = do
    -- Calibrated twice on purpose. The first pass is also the warm-up: it runs
    -- the work until a run of it is long enough to time, by which point
    -- whatever the engine was going to do to the code it has done. The second
    -- pass starts from that answer and grows it again if the warmed code turned
    -- out to be quicker -- which is the whole reason one untimed trial is not
    -- enough, since a trial that includes the cold run says the work costs more
    -- than it does and asks for too few repetitions.
    firstGuess <- calibrate 1 work
    per <- calibrate firstGuess work

    best <- Ref.new infinity
    total <- Ref.new 0.0
    shortest <- Ref.new infinity
    for_ (Array.range 1 rounds) \_ -> do
      elapsed <- sample per work
      Ref.modify_ (min elapsed) shortest
      let each = elapsed / Int.toNumber per
      Ref.modify_ (min each) best
      Ref.modify_ (_ + each) total

    fastest <- Ref.read best
    summed <- Ref.read total
    leanest <- Ref.read shortest
    Console.log
      ( "  " <> name <> "   best " <> ms fastest
          <> "   mean "
          <> ms (summed / Int.toNumber rounds)
          <> "   ("
          <> show per
          <> " per sample, shortest sample "
          <> ms leanest
          <> ", "
          <> show rounds
          <> " samples)"
      )

  -- How long `per` repetitions take.
  sample per work = do
    Milliseconds start <- unInstant <$> now
    let ran = repeat per work
    Milliseconds end <- unInstant <$> now
    pure (if ran then end - start else infinity)

  -- The smallest number of repetitions that takes at least `sampleMillis`,
  -- found by measuring rather than by dividing an estimate.
  -- Aimed a little over the floor, because a sample taken later can be a
  -- little quicker than the one that settled the count, and a floor that only
  -- just held is a floor that does not.
  calibrate from work = go from
    where
    target = sampleMillis * 1.1

    go per = do
      elapsed <- sample per work
      if elapsed >= target then pure per
      else go (grow per elapsed)

    -- Always at least one more, never more than eight times as many: a run
    -- that took no measurable time would otherwise ask for a number with no
    -- relation to anything.
    grow per elapsed =
      max (per + 1)
        ( min (per * 8)
            (Int.ceil (Int.toNumber per * target / max 0.25 elapsed))
        )

  -- The answer is carried through so that nothing is free to decide the work
  -- was not needed.
  repeat n work = go n true
    where
    go left ok
      | left <= 0 = ok
      | otherwise = go (left - 1) (ok && work unit)

  infinity = 1.0e30

  ms n = Format.toStringWith (Format.fixed 3) n <> "ms"
