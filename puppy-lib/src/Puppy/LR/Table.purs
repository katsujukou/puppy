-- | Turning the automaton into the tables the driver reads, and deciding what
-- | to do where more than one action wants the same cell.
-- |
-- | Every candidate for a cell is collected before any of them is settled.
-- | Settling them one pair at a time as they arrive makes the outcome depend on
-- | the order they arrived in: a `%nonassoc` that empties a cell would be
-- | undone by the next reduce to come along, which finds the cell empty and
-- | simply moves in.
-- |
-- | Deliberate disagreements are recorded alongside accidental ones. A `%left`
-- | declaration exists precisely so that a shift and a reduce can compete for a
-- | cell and the reduce win; telling those apart is the reporting layer's job,
-- | and it needs to see both.
module Puppy.LR.Table
  ( Action(..)
  , Cell
  , GotoCell
  , ConflictKind(..)
  , Preference(..)
  , Resolution(..)
  , Conflict
  , Table
  , tabulate
  , productionPrecedence
  , unresolved
  , conflictTerminal
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Puppy.LR.Automaton (Automaton)
import Puppy.LR.Grammar (LRGrammar, Precedence, Sym(..))
import Puppy.LR.Item (Item)
import Puppy.Syntax (Associativity(..))

data Action
  = Shift Int
  | Reduce Int
  | Accept

derive instance Eq Action
derive instance Ord Action

instance Show Action where
  show = case _ of
    Shift s -> "(Shift " <> show s <> ")"
    Reduce p -> "(Reduce " <> show p <> ")"
    Accept -> "Accept"

type Cell = { state :: Int, terminal :: Int }

type GotoCell = { state :: Int, nonterminal :: Int }

-- | What one production's precedence has to say about meeting the shift.
data Preference
  = PrefersShift
  | PrefersReduce
  | PrefersError
  | NoPreference

derive instance Eq Preference

instance Show Preference where
  show = case _ of
    PrefersShift -> "PrefersShift"
    PrefersReduce -> "PrefersReduce"
    PrefersError -> "PrefersError"
    NoPreference -> "NoPreference"

data ConflictKind
  = ShiftReduce { terminal :: Int, target :: Int, production :: Int }
  | Disputed
      { terminal :: Int
      , target :: Int
      , verdicts :: Array { production :: Int, prefers :: Preference }
      }
  -- ^ A shift facing several reduces whose precedences do not agree with one
  -- another. Kept as one conflict rather than several because there is one
  -- thing wrong here, and it is not that any single production lacks a
  -- declaration.
  | ReduceReduce { terminal :: Int, kept :: Int, dropped :: Int }
  | AcceptReduce { terminal :: Int, production :: Int }

derive instance Eq ConflictKind

instance Show ConflictKind where
  show = case _ of
    ShiftReduce r -> "(ShiftReduce " <> show r <> ")"
    Disputed r -> "(Disputed " <> show r <> ")"
    ReduceReduce r -> "(ReduceReduce " <> show r <> ")"
    AcceptReduce r -> "(AcceptReduce " <> show r <> ")"

data Resolution
  = ByPrecedence Action
  -- ^ A `%left`, `%right` or `%prec` said which one wins. The grammar asked
  -- for this, so it is not something to warn about.
  | ByNonassoc
  -- ^ `%nonassoc`: neither wins and the token becomes a parse error here,
  -- which is also what the grammar asked for.
  | ByDefault Action

-- ^ Nothing in the grammar decided it, so a rule of thumb did. This is the
-- one worth telling someone about. Every reduce/reduce disagreement lands
-- here: precedence has nothing to say about two completed productions.

derive instance Eq Resolution

instance Show Resolution where
  show = case _ of
    ByPrecedence a -> "(ByPrecedence " <> show a <> ")"
    ByNonassoc -> "ByNonassoc"
    ByDefault a -> "(ByDefault " <> show a <> ")"

type Conflict =
  { state :: Int
  , kind :: ConflictKind
  , resolution :: Resolution
  }

type Table =
  { action :: Map Cell Action
  , goto :: Map GotoCell Int
  , conflicts :: Array Conflict
  }

-- | The token two actions were competing for.
conflictTerminal :: Conflict -> Int
conflictTerminal conflict = case conflict.kind of
  ShiftReduce c -> c.terminal
  Disputed c -> c.terminal
  ReduceReduce c -> c.terminal
  AcceptReduce c -> c.terminal

-- | The conflicts the grammar did not ask for.
unresolved :: Table -> Array Conflict
unresolved table = Array.filter byDefault table.conflicts
  where
  byDefault c = case c.resolution of
    ByDefault _ -> true
    _ -> false

-- | A production's precedence is the one it was given by `%prec`, or failing
-- | that the one belonging to its last terminal -- the token whose arrival
-- | would be the last thing the production is still waiting for.
productionPrecedence :: LRGrammar -> Int -> Maybe Precedence
productionPrecedence g production = do
  prod <- Array.index g.productions production
  terminal <- case prod.precedence of
    Just t -> Just t
    Nothing -> lastTerminal prod.rhs
  Map.lookup terminal g.precedence

lastTerminal :: Array Sym -> Maybe Int
lastTerminal = Array.foldl step Nothing
  where
  step acc = case _ of
    T t -> Just t
    N _ -> acc

--------------------------------------------------------------------------------
-- Collecting what wants each cell
--------------------------------------------------------------------------------

type Gathered =
  { candidates :: Map Cell (Array Action)
  , goto :: Map GotoCell Int
  }

gather :: LRGrammar -> Automaton -> Gathered
gather g automaton = Array.foldl rows
  { candidates: Map.empty, goto: Map.empty }
  (Array.mapWithIndex Tuple automaton.states)
  where
  rows acc (Tuple state st) =
    let
      transitions = Map.toUnfoldable st.transitions :: Array (Tuple Sym Int)
      items = Map.toUnfoldable st.items :: Array (Tuple Item (Set Int))
    in
      Array.foldl (reduces state) (Array.foldl (moves state) acc transitions) items

  moves state acc (Tuple symbol target) = case symbol of
    N nonterminal -> acc
      { goto = Map.insert { state, nonterminal } target acc.goto }
    T terminal -> offer { state, terminal } (Shift target) acc

  reduces state acc (Tuple item lookaheads) =
    case Array.index g.productions item.production of
      Nothing -> acc
      Just prod
        | item.dot < Array.length prod.rhs -> acc
        | otherwise -> Array.foldl (reduceOn state item prod) acc
            (Array.fromFoldable lookaheads)

  -- A completed production added for a start symbol is not something to reduce
  -- by; reaching it with nothing left to read is the whole point.
  reduceOn state item prod acc terminal =
    let
      cell = { state, terminal }
    in
      case prod.source of
        Nothing | terminal == g.eof -> offer cell Accept acc
        _ -> offer cell (Reduce item.production) acc

  offer cell action acc = acc
    { candidates = Map.alter (Just <<< append action) cell acc.candidates }

  append action = case _ of
    Nothing -> [ action ]
    Just existing -> Array.snoc existing action

--------------------------------------------------------------------------------
-- Settling a cell
--------------------------------------------------------------------------------

type Settlement =
  { action :: Maybe Action
  , conflicts :: Array Conflict
  }

-- | Which of a shift and one reduce the grammar says should win, if it says.
data Winner
  = ShiftWins
  | ReduceWins Int
  | ErrorHere
  | Undecided

derive instance Eq Winner

-- | What the grammar has to say about a shift and one particular reduce.
decide :: LRGrammar -> Int -> Int -> Winner
decide g terminal production =
  case Map.lookup terminal g.precedence, productionPrecedence g production of
    Just token, Just rule
      | rule.level > token.level -> ReduceWins production
      | rule.level < token.level -> ShiftWins
      | otherwise -> case token.associativity of
          AssocLeft -> ReduceWins production
          AssocRight -> ShiftWins
          AssocNone -> ErrorHere
    _, _ -> Undecided

-- | Decide one cell, given everything that wanted it.
-- |
-- | There is at most one shift, since a state has at most one transition on a
-- | token, and at most one accept. What there can be many of is reduces, and a
-- | shift facing several of them has to be settled all at once rather than a
-- | pair at a time.
-- |
-- | The reason is that the reduces may never get to compete with each other. If
-- | the cell ends up as the shift, or as no action at all, no input reaches the
-- | point where the choice between them would matter, and reporting a
-- | reduce/reduce disagreement underneath it would be reporting something that
-- | cannot happen. That covers three of the five ways this goes:
-- |
-- |   * every reduce loses to the shift on precedence, so the cell is the
-- |     shift, deliberately;
-- |   * `%nonassoc` says the token belongs to none of them, so the cell is
-- |     empty, deliberately;
-- |   * nothing has a precedence at all, so the cell is the shift by the rule
-- |     of thumb that prefers shifting -- not what the grammar asked for, but
-- |     the same answer for every one of them, which is what matters here.
-- |
-- | The choice between the reduces becomes real only in the fourth case, where
-- | they all beat the shift; there it is the one thing left unsettled. In the
-- | fifth, their precedences disagree with one another, and that disagreement
-- | is the whole story -- so it is reported once, rather than once per
-- | production, each complaining of a declaration it may well already have.
-- |
-- | Every conflict in a cell carries the settlement of the cell itself, so a
-- | report can never claim the parser will do something the table does not say.
settle :: LRGrammar -> Cell -> Array Action -> Settlement
settle g cell candidates =
  if Array.any (_ == Accept) candidates then
    { action: Just Accept, conflicts: map acceptReduce reduces }
  else case Array.head shifts, Array.uncons (Array.sort reduces) of
    Nothing, Nothing -> { action: Nothing, conflicts: [] }
    Just target, Nothing -> { action: Just (Shift target), conflicts: [] }
    Nothing, Just { head: kept, tail: rest } ->
      { action: Just (Reduce kept)
      , conflicts: map (reduceReduce kept (ByDefault (Reduce kept))) rest
      }
    Just target, Just { head: kept, tail: rest } ->
      let
        ordered = Array.cons kept rest

        outcomes = map (decide g cell.terminal) ordered

        every w = Array.all (_ == w) outcomes

        everyReduce = Array.all isReduceWins outcomes

        onlyShifts resolution action =
          { action
          , conflicts: map (shiftReduce target resolution) ordered
          }
      in
        if every ShiftWins then
          onlyShifts (ByPrecedence (Shift target)) (Just (Shift target))
        else if every ErrorHere then
          onlyShifts ByNonassoc Nothing
        else if everyReduce then
          if Array.null rest then
            onlyShifts (ByPrecedence (Reduce kept)) (Just (Reduce kept))
          else
            -- Each of them beats the shift, deliberately. What nothing settles
            -- is which of them.
            { action: Just (Reduce kept)
            , conflicts: map (reduceReduce kept (ByDefault (Reduce kept))) rest
            }
        else if every Undecided then
          -- Nothing in the grammar speaks. They all lose to the shift by the
          -- same rule of thumb, so they never compete with each other either.
          onlyShifts (ByDefault (Shift target)) (Just (Shift target))
        else
          -- Declarations exist and disagree. Saying that once beats telling
          -- each production separately that it is missing a declaration it may
          -- well already have.
          { action: Just (Shift target)
          , conflicts:
              [ conflict
                  ( Disputed
                      { terminal: cell.terminal
                      , target
                      , verdicts: Array.zipWith verdict ordered outcomes
                      }
                  )
                  (ByDefault (Shift target))
              ]
          }
  where
  shifts = Array.mapMaybe onlyShift candidates

  onlyShift = case _ of
    Shift target -> Just target
    _ -> Nothing

  reduces = Array.mapMaybe onlyReduce candidates

  onlyReduce = case _ of
    Reduce p -> Just p
    _ -> Nothing

  isReduceWins = case _ of
    ReduceWins _ -> true
    _ -> false

  verdict production winner = { production, prefers: preference winner }

  preference = case _ of
    ShiftWins -> PrefersShift
    ReduceWins _ -> PrefersReduce
    ErrorHere -> PrefersError
    Undecided -> NoPreference

  conflict kind resolution = { state: cell.state, kind, resolution }

  acceptReduce production = conflict
    (AcceptReduce { terminal: cell.terminal, production })
    (ByDefault Accept)

  reduceReduce kept resolution dropped = conflict
    (ReduceReduce { terminal: cell.terminal, kept, dropped })
    resolution

  shiftReduce target resolution production = conflict
    (ShiftReduce { terminal: cell.terminal, target, production })
    resolution

tabulate :: LRGrammar -> Automaton -> Table
tabulate g automaton =
  let
    gathered = gather g automaton

    resolved = Array.foldl place { action: Map.empty, conflicts: [] }
      (Map.toUnfoldable gathered.candidates :: Array (Tuple Cell (Array Action)))
  in
    { action: resolved.action
    , goto: gathered.goto
    , conflicts: resolved.conflicts
    }
  where
  place acc (Tuple cell candidates) =
    let
      settlement = settle g cell candidates
    in
      { action: case settlement.action of
          Nothing -> acc.action
          Just action -> Map.insert cell action acc.action
      , conflicts: acc.conflicts <> settlement.conflicts
      }
