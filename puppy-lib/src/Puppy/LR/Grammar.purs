-- | The grammar as the automaton wants it: symbols are numbers, and there is a
-- | production to start from.
-- |
-- | Numbering is where names stop being useful. The tables the runtime reads
-- | are indexed by terminal and by nonterminal, so everything downstream works
-- | in integers; the names are kept alongside purely so that a conflict report
-- | or a parse error can be read by a person.
module Puppy.LR.Grammar
  ( Sym(..)
  , Prod
  , LRGrammar
  , LRError
  , Precedence
  , number
  , renderSym
  , renderProd
  ) where

import Prelude

import Prim hiding (Symbol)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Puppy.Grammar (Symbol(..))
import Puppy.Grammar as Core
import Data.Map (Map)
import Puppy.Syntax (Associativity, Span, eofToken)
import Puppy.Syntax as Syntax

data Sym
  = T Int
  | N Int

derive instance Eq Sym
derive instance Ord Sym

instance Show Sym where
  show = case _ of
    T i -> "(T " <> show i <> ")"
    N i -> "(N " <> show i <> ")"

type Prod =
  { lhs :: Int
  , rhs :: Array Sym
  , source :: Maybe Int
  -- ^ Which of the core grammar's productions this came from, and so where its
  -- semantic action lives. `Nothing` for the productions added below to give
  -- each start symbol something to be reduced by.
  , precedence :: Maybe Int
  -- ^ The terminal named by `%prec`, resolved.
  , span :: Span
  }

type LRGrammar =
  { terminals :: Array { name :: String, display :: String }
  , nonterminals :: Array String
  , productions :: Array Prod
  , starts ::
      Array
        { name :: String
        , symbol :: Int
        -- ^ The nonterminal the entry state has a transition on.
        , production :: Int
        -- ^ The added production whose completed item means acceptance.
        }
  , precedence :: Map Int Precedence
  -- ^ Only the terminals a `%left`, `%right` or `%nonassoc` named.
  , eof :: Int
  }

-- | Where a terminal sits in the pecking order, and which way it leans when it
-- | meets something of its own rank. The level is the position of the
-- | declaration that named it: later declarations bind more tightly.
type Precedence =
  { level :: Int
  , associativity :: Associativity
  }

type LRError = { message :: String, span :: Span }

renderSym :: LRGrammar -> Sym -> String
renderSym g = case _ of
  T i -> maybe' (map _.name (Array.index g.terminals i)) ("t" <> show i)
  N i -> maybe' (Array.index g.nonterminals i) ("n" <> show i)
  where
  maybe' m fallback = fromMaybe fallback m

renderProd :: LRGrammar -> Prod -> String
renderProd g p =
  fromMaybe "?" (Array.index g.nonterminals p.lhs) <> " -> " <>
    if Array.null p.rhs then "<empty>"
    else joinWith " " (map (renderSym g) p.rhs)

-- | The pseudo-nonterminal that exists only to be reduced to at the end of a
-- | parse. Its name is spelled so that no grammar can write it: an identifier
-- | cannot contain angle brackets.
startName :: String -> String
startName symbol = "<start " <> symbol <> ">"

number :: Core.Grammar -> Either (Array LRError) LRGrammar
number core = do
  productions <- traverse convert (Array.mapWithIndex Tuple core.productions)
  let
    augmented = Array.mapMaybe augment core.starts
    all = productions <> map _.production augmented
    g =
      { terminals
      , nonterminals
      , productions: all
      , precedence
      , starts: Array.mapWithIndex
          (\i a -> a.start { production = Array.length productions + i })
          augmented
      , eof
      }
  case unproductive (Array.length declared) g of
    [] -> Right g
    errors -> Left errors
  where
  terminals =
    map (\t -> { name: t.name, display: t.display })
      (Syntax.declaredTokens core.tokens)
      <> [ { name: eofToken.name, display: eofToken.display } ]

  eof = Array.length terminals - 1

  -- The order the declarations were written in is the whole of their meaning,
  -- so the index is the level.
  precedence = Map.fromFoldable
    (Array.concat (Array.mapWithIndex ranked core.precedences))

  ranked level decl = Array.mapMaybe
    ( \name -> map
        (\t -> Tuple t { level, associativity: decl.associativity })
        (Map.lookup name terminalIndex)
    )
    decl.tokens

  terminalIndex = indexOf (map _.name terminals)

  -- Every nonterminal the productions mention, in the order they first turn up.
  declared = Array.nub (Array.concatMap mentions core.productions)

  nonterminals = declared <> map (startName <<< _.symbol) core.starts

  mentions p = Array.cons p.lhs (Array.mapMaybe nonterminalName p.rhs)

  nonterminalName = case _ of
    Nonterminal n -> Just n
    Terminal _ -> Nothing

  nonterminalIndex = indexOf nonterminals

  indexOf names = Map.fromFoldable (Array.mapWithIndex (flip Tuple) names)

  convert (Tuple i p) = do
    lhs <- require nonterminalIndex p.lhs "nonterminal" p.span
    rhs <- traverse (toSym p.span) p.rhs
    prec <- traverse
      (\t -> require terminalIndex t "token" p.span)
      p.precedence
    pure { lhs, rhs, source: Just i, precedence: prec, span: p.span }

  toSym span = case _ of
    Terminal t -> T <$> require terminalIndex t "token" span
    Nonterminal n -> N <$> require nonterminalIndex n "nonterminal" span

  -- These lookups cannot fail: the tables above are built from exactly the
  -- names the productions use. Saying so out loud beats a silent default.
  require table name what span = case Map.lookup name table of
    Just i -> Right i
    Nothing -> Left
      [ { message: "internal error: " <> what <> " `" <> name
            <> "` was never numbered"
        , span
        }
      ]

  -- `<start S> -> S`, whose completed item is what acceptance means. The end of
  -- input is a lookahead on that item rather than a symbol of the production,
  -- so the driver accepts without consuming it.
  augment s = do
    nonterminal <- Map.lookup (startName s.symbol) nonterminalIndex
    symbol <- Map.lookup s.symbol nonterminalIndex
    pure
      { production:
          { lhs: nonterminal
          , rhs: [ N symbol ]
          , source: Nothing
          , precedence: Nothing
          , span: s.span
          }
      , start: { name: s.symbol, symbol, production: 0 }
      }

-- | Nonterminals that derive no sequence of tokens at all.
-- |
-- | `declared` is how many of them the grammar actually wrote. The productions
-- | added for the start symbols take part in the fixpoint, but their
-- | nonterminals are named so that no grammar could have written them, so
-- | naming one in a diagnostic would point at something the reader cannot see.
-- |
-- | A rule whose every alternative needs itself -- `expr: | expr PLUS expr` and
-- | nothing else -- is well formed in every way checked so far, and yet no
-- | input can ever match it. Left unreported it becomes a pile of unreachable
-- | states and a conflict report about a rule that could never have worked.
unproductive :: Int -> LRGrammar -> Array LRError
unproductive declared g = Array.mapMaybe report reportable
  where
  reportable = if declared <= 0 then [] else Array.range 0 (declared - 1)

  productive = fixpoint Set.empty

  fixpoint known =
    let
      grown = Array.foldl step known g.productions
    in
      if Set.size grown == Set.size known then known else fixpoint grown

  step known p
    | Set.member p.lhs known = known
    | Array.all (derives known) p.rhs = Set.insert p.lhs known
    | otherwise = known

  derives known = case _ of
    T _ -> true
    N n -> Set.member n known

  report n
    | Set.member n productive = Nothing
    | otherwise = do
        name <- Array.index g.nonterminals n
        first <- Array.find (\p -> p.lhs == n) g.productions
        pure
          { message: "`" <> name
              <> "` cannot derive any sequence of tokens, so nothing can ever match it"
          , span: first.span
          }
