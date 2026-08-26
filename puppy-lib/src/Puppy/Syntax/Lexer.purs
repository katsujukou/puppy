-- | Turning a `.pursy` file into a flat token stream.
-- |
-- | The only hard part is finding where an embedded fragment of PureScript
-- | ends, and `scanBraced` is where that happens.
-- |
-- | Both kinds of fragment -- a semantic action and a type annotation -- are
-- | delimited by braces, and the lexer does not try to tell them apart: a type
-- | can only appear in the declaration section and an action only in the rules
-- | section, so position decides, and that is the parser's business.
-- |
-- | Braces are not an arbitrary choice. In PureScript `{` and `}` are reserved
-- | punctuation and can never be part of an operator, so counting them (while
-- | skipping strings and comments) locates the end of a fragment exactly. An
-- | angle-bracketed `<...>`, the more familiar spelling, cannot: `>` is a
-- | symbol character, so `f ~> g` or `(>) a b` would be cut short, and no
-- | amount of special-casing `->` and `=>` fixes the general case.
module Puppy.Syntax.Lexer
  ( LexToken(..)
  , Braced
  , Located
  , LexError
  , lex
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits as SCU
import Puppy.Syntax (Code, Pos, Span)

-- | A `{ ... }` fragment, and where inside it a `$$` stood.
-- |
-- | Only a token pattern has any use for the placeholders; every other kind of
-- | fragment is a fragment with an empty array beside it.
type Braced = { code :: Code, placeholders :: Array Span }

data LexToken
  = TIdent String
  | TKeyword String
  -- ^ A `%` directive, without the `%`: `token`, `start`, `left`, ...
  | THeader Code
  -- ^ `%{ ... %}`
  | TSeparator
  -- ^ `%%`
  | TBraced Braced
  -- ^ `{ ... }` -- a semantic action, a type annotation or a token pattern,
  -- depending on where it turned up.
  | TString String
  -- ^ A quoted string, used for a token's display name.
  | TColon
  | TBar
  | TComma
  | TEquals
  | TSemi
  | TAt
  -- ^ `@` before a token pattern: the value is the whole token rather than a
  -- part picked out by `$$`.
  | TLParen
  | TRParen
  | TEnd

derive instance Eq LexToken

instance Show LexToken where
  show = case _ of
    TIdent s -> "(TIdent " <> show s <> ")"
    TKeyword s -> "(TKeyword " <> show s <> ")"
    THeader c -> "(THeader " <> show c <> ")"
    TSeparator -> "TSeparator"
    TBraced c -> "(TBraced " <> show c <> ")"
    TString s -> "(TString " <> show s <> ")"
    TColon -> "TColon"
    TBar -> "TBar"
    TComma -> "TComma"
    TEquals -> "TEquals"
    TSemi -> "TSemi"
    TAt -> "TAt"
    TLParen -> "TLParen"
    TRParen -> "TRParen"
    TEnd -> "TEnd"

type Located = { token :: LexToken, span :: Span }

type LexError = { message :: String, pos :: Pos }

--------------------------------------------------------------------------------
-- Character classes
--------------------------------------------------------------------------------

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

isAlpha :: Char -> Boolean
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isIdentStart :: Char -> Boolean
isIdentStart c = isAlpha c || c == '_'

isIdentChar :: Char -> Boolean
isIdentChar c = isAlpha c || isDigit c || c == '_' || c == '\''

-- | The characters PureScript allows in an operator. Used to tell a run of
-- | dashes that opens a comment from one that is part of an operator.
isSymbolChar :: Char -> Boolean
isSymbolChar c = Array.elem c
  [ ':'
  , '!'
  , '#'
  , '$'
  , '%'
  , '&'
  , '*'
  , '+'
  , '.'
  , '/'
  , '<'
  , '='
  , '>'
  , '?'
  , '@'
  , '\\'
  , '^'
  , '|'
  , '-'
  , '~'
  ]

--------------------------------------------------------------------------------
-- Positions
--------------------------------------------------------------------------------

origin :: Pos
origin = { offset: 0, line: 1, column: 1 }

step :: Array Char -> Pos -> Pos
step chars p = case Array.index chars p.offset of
  Just '\n' -> { offset: p.offset + 1, line: p.line + 1, column: 1 }
  _ -> { offset: p.offset + 1, line: p.line, column: p.column + 1 }

bump :: Array Char -> Int -> Pos -> Pos
bump chars n p
  | n <= 0 = p
  | otherwise = bump chars (n - 1) (step chars p)

takeWhileFrom :: Array Char -> (Char -> Boolean) -> Pos -> Pos
takeWhileFrom chars ok p = case Array.index chars p.offset of
  Just c | ok c -> takeWhileFrom chars ok (step chars p)
  _ -> p

slice :: Array Char -> Int -> Int -> String
slice chars from to = SCU.fromCharArray (Array.slice from to chars)

--------------------------------------------------------------------------------
-- Scanning embedded PureScript
--------------------------------------------------------------------------------

-- | What the scanner is in the middle of. Braces are only counted in
-- | `ModeNormal`; every other mode exists precisely so that a brace inside it
-- | is ignored.
data ScanMode
  = ModeNormal
  | ModeString
  | ModeTripleString
  | ModeChar
  | ModeLineComment
  | ModeBlockComment Int

-- | Scan from an opening brace to its match, skipping over anything that only
-- | looks like one. Braces nest freely -- record literals, record types and
-- | `do` blocks all produce them -- so the fragment ends where the depth
-- | returns to zero.
-- |
-- | The returned span covers the text between the braces, not the braces
-- | themselves, since that text is what ends up in generated code.
-- |
-- | `placeholders` is where `$$` appeared. A token pattern uses it to say where
-- | a payload sits; every other kind of fragment simply has none, and ignores
-- | the field. Recording it here rather than searching the finished text is the
-- | whole point: this scanner already knows a string literal from code, and a
-- | `$$` inside one is not a placeholder.
scanBraced
  :: Array Char
  -> Pos
  -> Either LexError { code :: Code, placeholders :: Array Span, next :: Pos }
scanBraced chars start = loop inner ModeNormal 1 '{' Nil
  where
  inner = step chars start

  peek n p = Array.index chars (p.offset + n)

  unterminated :: forall a. Either LexError a
  unterminated = Left { message: "unterminated `{ ... }` block", pos: start }

  finish p holes = Right
    { code:
        { text: slice chars inner.offset p.offset
        , span: { start: inner, end: p }
        }
    , placeholders: Array.fromFoldable (List.reverse holes)
    , next: step chars p
    }

  -- A `$$` is a placeholder only when it is a token of its own. PureScript
  -- operators are a maximal run of symbol characters, so a symbol character on
  -- either side makes this part of something else -- `$$$`, `<$$>` -- and not
  -- a hole at all.
  standalone p prev =
    peek 1 p == Just '$'
      && not (isSymbolChar prev)
      && not (maybe false isSymbolChar (peek 2 p))

  loop
    :: Pos
    -> ScanMode
    -> Int
    -> Char
    -> List Span
    -> Either LexError { code :: Code, placeholders :: Array Span, next :: Pos }
  loop p mode depth prev holes = case Array.index chars p.offset of
    Nothing -> unterminated
    Just c -> case mode of
      ModeLineComment ->
        loop (step chars p) (if c == '\n' then ModeNormal else mode) depth c holes

      ModeBlockComment n
        | c == '{' && peek 1 p == Just '-' ->
            loop (bump chars 2 p) (ModeBlockComment (n + 1)) depth '-' holes
        | c == '-' && peek 1 p == Just '}' ->
            loop (bump chars 2 p)
              (if n == 1 then ModeNormal else ModeBlockComment (n - 1))
              depth
              '}'
              holes
        | otherwise -> loop (step chars p) mode depth c holes

      ModeString
        | c == '\\' -> loop (bump chars 2 p) mode depth '\\' holes
        | c == '"' -> loop (step chars p) ModeNormal depth c holes
        | otherwise -> loop (step chars p) mode depth c holes

      -- Triple-quoted strings are raw: a backslash in one is just a backslash.
      ModeTripleString
        | c == '"' && peek 1 p == Just '"' && peek 2 p == Just '"' ->
            loop (bump chars 3 p) ModeNormal depth '"' holes
        | otherwise -> loop (step chars p) mode depth c holes

      ModeChar
        | c == '\\' -> loop (bump chars 2 p) mode depth '\\' holes
        | c == '\'' -> loop (step chars p) ModeNormal depth c holes
        | otherwise -> loop (step chars p) mode depth c holes

      ModeNormal
        | c == '"' ->
            if peek 1 p == Just '"' && peek 2 p == Just '"' then
              loop (bump chars 3 p) ModeTripleString depth '"' holes
            else
              loop (step chars p) ModeString depth c holes

        -- A quote straight after an identifier character is a prime, as in
        -- `xs'`, not the opening of a character literal.
        | c == '\'' && isIdentChar prev -> loop (step chars p) mode depth c holes
        | c == '\'' -> loop (step chars p) ModeChar depth c holes

        -- Checked before the opening brace, so that `{-` opens a comment
        -- rather than nesting a brace.
        | c == '{' && peek 1 p == Just '-' ->
            loop (bump chars 2 p) (ModeBlockComment 1) depth '-' holes

        | c == '-' && peek 1 p == Just '-' -> dashes p depth holes

        | c == '$' && standalone p prev ->
            let
              after = bump chars 2 p
            in
              loop after mode depth '$' ({ start: p, end: after } : holes)

        | c == '{' -> loop (step chars p) mode (depth + 1) c holes
        | c == '}' ->
            if depth == 1 then finish p holes
            else loop (step chars p) mode (depth - 1) c holes

        | otherwise -> loop (step chars p) mode depth c holes

  -- Two or more dashes open a line comment unless a symbol character follows
  -- the run, which makes the whole thing an operator such as `-->`.
  dashes p depth holes =
    let
      end = takeWhileFrom chars (_ == '-') p
      after = maybe false isSymbolChar (Array.index chars end.offset)
    in
      loop end (if after then ModeNormal else ModeLineComment) depth '-' holes

-- | `%{ ... %}`. Unlike a braced fragment this is scanned for a literal `%}`:
-- | the header is arbitrary top-of-file PureScript, and `%}` is not a sequence
-- | that occurs in it by accident.
scanHeader :: Array Char -> Pos -> Either LexError { code :: Code, next :: Pos }
scanHeader chars start = loop inner
  where
  inner = bump chars 2 start

  loop p = case Array.index chars p.offset, Array.index chars (p.offset + 1) of
    Nothing, _ -> Left { message: "unterminated `%{` header", pos: start }
    Just '%', Just '}' -> Right
      { code:
          { text: slice chars inner.offset p.offset
          , span: { start: inner, end: p }
          }
      , next: bump chars 2 p
      }
    _, _ -> loop (step chars p)

-- | A quoted string in the grammar itself, used for a token's display name.
scanString :: Array Char -> Pos -> Either LexError { text :: String, next :: Pos }
scanString chars start = loop (step chars start)
  where
  loop p = case Array.index chars p.offset of
    Nothing -> Left { message: "unterminated string", pos: start }
    Just '\\' -> loop (bump chars 2 p)
    Just '"' -> Right
      { text: slice chars (start.offset + 1) p.offset
      , next: step chars p
      }
    Just _ -> loop (step chars p)

-- | A nestable `{- ... -}` comment in the grammar itself.
skipBlockComment :: Array Char -> Pos -> Either LexError Pos
skipBlockComment chars start = loop (bump chars 2 start) 1
  where
  loop p depth =
    case Array.index chars p.offset, Array.index chars (p.offset + 1) of
      Nothing, _ -> Left { message: "unterminated block comment", pos: start }
      Just '{', Just '-' -> loop (bump chars 2 p) (depth + 1)
      Just '-', Just '}' ->
        if depth == 1 then Right (bump chars 2 p)
        else loop (bump chars 2 p) (depth - 1)
      _, _ -> loop (step chars p) depth

skipLineComment :: Array Char -> Pos -> Pos
skipLineComment chars p = case Array.index chars p.offset of
  Nothing -> p
  Just '\n' -> step chars p
  Just _ -> skipLineComment chars (step chars p)

--------------------------------------------------------------------------------
-- The lexer proper
--------------------------------------------------------------------------------

-- | Tokenise a grammar file. The result always ends with `TEnd`, so the parser
-- | never has to distinguish "no more tokens" from "ran off the array".
lex :: String -> Either LexError (Array Located)
lex source = map (Array.fromFoldable <<< List.reverse) (go origin Nil)
  where
  chars = SCU.toCharArray source

  peek n p = Array.index chars (p.offset + n)

  located token start end = { token, span: { start, end } }

  go :: Pos -> List Located -> Either LexError (List Located)
  go pos acc = case Array.index chars pos.offset of
    Nothing -> Right (located TEnd pos pos : acc)
    Just c
      | isSpace c -> go (step chars pos) acc

      | c == '-' && peek 1 pos == Just '-' -> go (skipLineComment chars pos) acc

      | c == '{' && peek 1 pos == Just '-' ->
          case skipBlockComment chars pos of
            Left err -> Left err
            Right next -> go next acc

      | c == '{' ->
          case scanBraced chars pos of
            Left err -> Left err
            Right r ->
              go r.next
                ( located (TBraced { code: r.code, placeholders: r.placeholders })
                    pos
                    r.next : acc
                )

      | c == '"' ->
          case scanString chars pos of
            Left err -> Left err
            Right r -> go r.next (located (TString r.text) pos r.next : acc)

      | c == '%' -> percent pos acc

      | isIdentStart c ->
          let
            end = takeWhileFrom chars isIdentChar pos
          in
            go end
              (located (TIdent (slice chars pos.offset end.offset)) pos end : acc)

      | otherwise ->
          let
            next = step chars pos

            single tok = go next (located tok pos next : acc)
          in
            case c of
              ':' -> single TColon
              '|' -> single TBar
              ',' -> single TComma
              '=' -> single TEquals
              ';' -> single TSemi
              '@' -> single TAt
              '(' -> single TLParen
              ')' -> single TRParen
              _ -> Left { message: "unexpected character " <> show c, pos }

  percent pos acc = case peek 1 pos of
    Just '{' ->
      case scanHeader chars pos of
        Left err -> Left err
        Right r -> go r.next (located (THeader r.code) pos r.next : acc)

    Just '%' ->
      let
        next = bump chars 2 pos
      in
        go next (located TSeparator pos next : acc)

    Just c | isIdentStart c ->
      let
        end = takeWhileFrom chars isIdentChar (step chars pos)
      in
        go end
          ( located (TKeyword (slice chars (pos.offset + 1) end.offset)) pos end
              : acc
          )

    _ -> Left { message: "expected a directive name after `%`", pos }
