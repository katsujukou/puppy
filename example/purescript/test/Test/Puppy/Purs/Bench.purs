-- | How fast the generated PureScript parser is, on a token stream and nothing
-- | else.
-- |
-- | There is no lexer here on purpose. What is being measured is the parser --
-- | the table lookups, the stack, the semantic actions -- and a lexer in front
-- | of it would put its own cost in the same number. The tokens are built once,
-- | before the clock starts, and the same array is parsed every round.
-- |
-- | Run it with:
-- |
-- | ```sh
-- | spago test -p puppy-example-purescript -m Test.Puppy.Purs.Bench --pure
-- | ```
module Test.Puppy.Purs.Bench where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), isRight)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Number.Format as Format
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Now (now)
import Effect.Ref as Ref
import Puppy.Purs.Parser as P
import Puppy.Purs.Token as T

at :: T.Pos
at = { line: 1, column: 1 }

lo :: String -> T.Token
lo name = T.TokLower { pos: at, name }

up :: String -> T.Token
up name = T.TokUpper { pos: at, name }

opr :: String -> T.Token
opr name = T.TokOperator { pos: at, name }

int :: Int -> T.Token
int value = T.TokInt { pos: at, raw: show value, value }

-- | One declaration pair, the shape most of a real module is made of: a
-- | signature and a binding whose body mixes application with operators.
-- |
-- | ```purescript
-- | fooN :: Int -> Int
-- | fooN x = f x <> g 1 <> h 2
-- | ```
declaration :: Int -> Array T.Token
declaration i =
  [ lo name
  , T.TokDoubleColon at
  , up "Int"
  , T.TokRightArrow at
  , up "Int"
  , T.TokLayoutSep at
  , lo name
  , lo "x"
  , T.TokEquals at
  , lo "f"
  , lo "x"
  , opr "<>"
  , lo "g"
  , int 1
  , opr "<>"
  , lo "h"
  , int 2
  ]
  where
  name = "foo" <> show i

-- | A module of `n` declaration pairs, with the header and the layout tokens a
-- | lexer would have inserted.
moduleOf :: Int -> Array T.Token
moduleOf n =
  [ lo "module", up "Bench", lo "where", T.TokLayoutStart at ]
    <> Array.intercalate [ T.TokLayoutSep at ]
      (map declaration (Array.range 1 n))
    <> [ T.TokLayoutEnd at ]

main :: Effect Unit
main = do
  let
    declarations = 500
    rounds = 20
    input = moduleOf declarations
    tokens = Array.length input

  -- Fail loudly rather than timing an error path.
  case P.parseModule input of
    Left err -> Console.error
      ("the input does not parse: token " <> show err.position)
    Right _ -> do
      Console.log
        ( "parsing " <> show tokens <> " tokens ("
            <> show declarations
            <> " declarations), "
            <> show rounds
            <> " rounds"
        )

      best <- Ref.new infinity
      total <- Ref.new 0.0

      for_ (Array.range 1 rounds) \_ -> do
        Milliseconds start <- unInstant <$> now
        -- Kept and looked at, so that nothing is free to drop the work.
        let answered = isRight (P.parseModule input)
        Milliseconds end <- unInstant <$> now
        let elapsed = if answered then end - start else 0.0
        Ref.modify_ (min elapsed) best
        Ref.modify_ (_ + elapsed) total

      fastest <- Ref.read best
      summed <- Ref.read total
      Console.log
        ( "  best " <> ms fastest
            <> "   mean "
            <> ms (summed / Int.toNumber rounds)
            <> "   "
            <> rate tokens fastest
        )
  where
  infinity = 1.0e30

  ms n = Format.toStringWith (Format.fixed 2) n <> "ms"

  rate tokens elapsed
    | elapsed <= 0.0 = "too fast to rate"
    | otherwise =
        Format.toStringWith (Format.fixed 0)
          (Int.toNumber tokens / elapsed * 1000.0)
          <> " tokens/s"
