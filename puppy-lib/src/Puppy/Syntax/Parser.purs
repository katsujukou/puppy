-- | Recursive descent over the token stream produced by `Puppy.Syntax.Lexer`.
-- |
-- | The result mirrors the source. No name is resolved, no parameterised rule
-- | is expanded and no conflict is looked for here; the one piece of meaning
-- | this module enforces is that the reserved end-of-input name stays reserved,
-- | because that is cheapest to report where the span is still at hand.
-- |
-- | A `{ ... }` block means a type in the declaration section and a semantic
-- | action in the rules section. The lexer does not distinguish the two, so
-- | deciding which is which is one of this module's jobs.
module Puppy.Syntax.Parser
  ( ParseError
  , parseGrammar
  , parse
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Traversable (traverse)
import Puppy.Syntax
  ( Associativity(..)
  , Code
  , ConflictDirective(..)
  , Element
  , Grammar
  , Pos
  , PrecedenceDecl
  , Production
  , Rule
  , Span
  , StartDecl
  , SymbolRef(..)
  , TokenDecl
  , TokenPattern
  , TokenSource(..)
  , TokenValue(..)
  , TypeDecl
  , DeriveDecl
  , eofToken
  )
import Puppy.Syntax.Lexer (LexToken(..), Located)
import Puppy.Syntax.Lexer as Lexer

type ParseError = { message :: String, span :: Span }

type Named = { name :: String, span :: Span }

--------------------------------------------------------------------------------
-- A very small parser monad over the token array
--------------------------------------------------------------------------------

newtype Parser a =
  Parser (Array Located -> Int -> Either ParseError { value :: a, next :: Int })

unParser
  :: forall a
   . Parser a
  -> Array Located
  -> Int
  -> Either ParseError { value :: a, next :: Int }
unParser (Parser p) = p

instance Functor Parser where
  map f p = Parser \toks i ->
    map (\r -> { value: f r.value, next: r.next }) (unParser p toks i)

instance Apply Parser where
  apply = ap

instance Applicative Parser where
  pure a = Parser \_ i -> Right { value: a, next: i }

instance Bind Parser where
  bind p f = Parser \toks i -> case unParser p toks i of
    Left err -> Left err
    Right r -> unParser (f r.value) toks r.next

instance Monad Parser

nowhere :: Pos
nowhere = { offset: 0, line: 1, column: 1 }

-- | Repeat a step until it declines, without a stack frame per repetition.
-- |
-- | The obvious spelling -- `do x <- step; loop (x : acc)` -- puts the
-- | recursive call under a bind, where it stops being a tail call. One frame
-- | per repetition is nothing for the symbols of a production and fatal for a
-- | file's worth of rules.
repeatedly :: forall a. (a -> Parser (Maybe a)) -> a -> Parser a
repeatedly step seed = Parser \toks i0 -> go toks seed i0
  where
  go toks acc i = case unParser (step acc) toks i of
    Left err -> Left err
    Right r -> case r.value of
      Nothing -> Right { value: acc, next: r.next }
      Just grown -> go toks grown r.next

fatal :: forall a. String -> Span -> Parser a
fatal message span = Parser \_ _ -> Left { message, span }

-- | The token under the cursor, without consuming it.
-- |
-- | Running off the end is an error rather than a repeat of the last token.
-- | Repeating it would let a loop that consumes identifiers spin forever on a
-- | stream that never reaches `TEnd`; `parseGrammar` rejects such a stream up
-- | front, and this makes the guarantee independent of that check.
current :: Parser Located
current = Parser \toks i -> case Array.index toks i of
  Just t -> Right { value: t, next: i }
  Nothing -> Left
    { message: "unexpected end of the token stream"
    , span: lastSpan toks
    }

lastSpan :: Array Located -> Span
lastSpan toks = case Array.last toks of
  Just t -> t.span
  Nothing -> { start: nowhere, end: nowhere }

advance :: Parser Unit
advance = Parser \_ i -> Right { value: unit, next: i + 1 }

-- | Consume the token under the cursor if it is the one expected.
expect :: LexToken -> String -> Parser Unit
expect wanted what = do
  t <- current
  if t.token == wanted then advance
  else fatal ("expected " <> what <> ", found " <> describe t.token) t.span

describe :: LexToken -> String
describe = case _ of
  TIdent name -> "`" <> name <> "`"
  TKeyword name -> "`%" <> name <> "`"
  THeader _ -> "a `%{ ... %}` header"
  TSeparator -> "`%%`"
  TBraced _ -> "a `{ ... }` block"
  TString s -> "the string " <> show s
  TColon -> "`:`"
  TBar -> "`|`"
  TComma -> "`,`"
  TEquals -> "`=`"
  TSemi -> "`;`"
  TAt -> "`@`"
  TLParen -> "`(`"
  TRParen -> "`)`"
  TEnd -> "end of file"

--------------------------------------------------------------------------------
-- Small building blocks
--------------------------------------------------------------------------------

-- | Close off a "first item, then a reverse-accumulated tail" loop. Reversing
-- | the whole of `first : rest` instead is the obvious mistake: it sends the
-- | first item to the end.
finish1 :: forall a. a -> List a -> Array a
finish1 first rest = Array.fromFoldable (first : List.reverse rest)

identifier :: String -> Parser Named
identifier what = do
  t <- current
  case t.token of
    TIdent name -> advance $> { name, span: t.span }
    _ -> fatal ("expected " <> what <> ", found " <> describe t.token) t.span

-- | One or more identifiers, as every declaration that names symbols accepts.
identifiers1 :: String -> Parser (Array Named)
identifiers1 what = do
  first <- identifier what
  rest <- repeatedly step Nil
  pure (finish1 first rest)
  where
  step acc = do
    t <- current
    case t.token of
      TIdent name -> do
        advance
        pure (Just ({ name, span: t.span } : acc))
      _ -> pure Nothing

-- | A `{ ... }` in a position where a type may or may not appear.
optionalType :: Parser (Maybe Code)
optionalType = do
  t <- current
  case t.token of
    TBraced braced -> advance $> Just braced.code
    _ -> pure Nothing

requiredType :: String -> Parser Code
requiredType what = do
  t <- current
  case t.token of
    TBraced braced -> advance $> braced.code
    _ -> fatal
      ( "expected a `{ ... }` type annotation after " <> what <> ", found "
          <> describe t.token
      )
      t.span

-- | A `{ ... }` in a position where a token pattern may or may not appear.
-- |
-- | The placeholders come from the lexer, which knew a `$$` in code from one
-- | inside a string. Nothing looks at this text again.
optionalPattern :: Parser (Maybe TokenPattern)
optionalPattern = do
  t <- current
  case t.token of
    TBraced braced ->
      advance $> Just { code: braced.code, holes: braced.placeholders }
    _ -> pure Nothing

-- | The end-of-input terminal belongs to Puppy, not to the grammar. Every
-- | position where a grammar can name a symbol has to say so, or the promise
-- | that `EOF` is reserved is only half kept.
checkNotReserved :: String -> Span -> String -> Parser Unit
checkNotReserved name span role =
  when (name == eofToken.name) do
    fatal
      ( "`" <> eofToken.name
          <> "` is reserved for end of input and cannot be "
          <> role
          <> "; Puppy declares it and the generated wrapper appends it to the token stream"
      )
      span

checkNoneReserved :: String -> Array Named -> Parser Unit
checkNoneReserved role = traverse_ \n -> checkNotReserved n.name n.span role

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

-- | A `%token` before the grammar's mode is known.
-- |
-- | Whether a pattern belongs here cannot be decided while reading, because
-- | `%tokentype` may not have been read yet: declarations are in any order.
type PendingToken =
  { decl :: TokenDecl
  , pattern :: Maybe TokenPattern
  , whole :: Maybe Span
  -- ^ The span of the `@` that said the value is the whole token, if one was
  -- written. Only external mode has anywhere to put it; `tokenSource` is where
  -- that is found out.
  }

type Decls =
  { tokenType :: Maybe Code
  , tokens :: List PendingToken
  , starts :: List StartDecl
  , types :: List TypeDecl
  , derives :: List DeriveDecl
  , precedences :: List PrecedenceDecl
  }

emptyDecls :: Decls
emptyDecls =
  { tokenType: Nothing
  , tokens: Nil
  , starts: Nil
  , types: Nil
  , precedences: Nil
  , derives: Nil
  }

declarations :: Decls -> Parser Decls
declarations = repeatedly step
  where
  step acc = do
    t <- current
    case t.token of
      TSeparator -> pure Nothing
      TEnd -> fatal "expected `%%` before the end of the file" t.span
      TKeyword "token" -> advance *> map Just (tokenDecls acc)
      TKeyword "tokentype" ->
        advance *> map Just (tokenTypeDecl t.span acc)
      TKeyword "start" -> advance *> map Just (startDecls acc)
      TKeyword "type" -> advance *> map Just (typeDecls acc)
      TKeyword "derive" -> advance *> map Just (deriveDecls acc)
      TKeyword "left" ->
        advance *> map Just (precedenceDecls AssocLeft t.span acc)
      TKeyword "right" ->
        advance *> map Just (precedenceDecls AssocRight t.span acc)
      TKeyword "nonassoc" ->
        advance *> map Just (precedenceDecls AssocNone t.span acc)
      _ -> fatal
        ("unexpected " <> describe t.token <> " in the declaration section")
        t.span

-- | `%tokentype { Type }`
tokenTypeDecl :: Span -> Decls -> Parser Decls
tokenTypeDecl span acc = do
  ty <- requiredType "`%tokentype`"
  when (isJust acc.tokenType) do
    fatal "`%tokentype` is declared more than once" span
  pure acc { tokenType = Just ty }

-- | `%token { Payload }? (NAME "display"? @? { Pattern }?)+`
tokenDecls :: Decls -> Parser Decls
tokenDecls acc = do
  payload <- optionalType
  read <- repeatedly (step payload) { found: Nil, first: true }
  pure acc { tokens = read.found <> acc.tokens }
  where
  step payload read = do
    t <- current
    case t.token of
      TIdent name -> do
        advance
        checkNotReserved name t.span "declared"
        display <- displayName name
        whole <- optionalAt payload
        pattern <- optionalPattern
        refuseBothMarks whole pattern
        pure
          ( Just read
              { found =
                  { decl:
                      { name, constructor: name, display, payload, span: t.span }
                  , pattern
                  , whole
                  } : read.found
              , first = false
              }
          )
      _
        | read.first ->
            fatal ("expected a token name, found " <> describe t.token) t.span
        | otherwise -> pure Nothing

  -- `@` says the value is the whole token, whose type is the one `%tokentype`
  -- named. There is nothing left for a payload type to say, so a declaration
  -- that has one has already said something else.
  optionalAt payload = do
    t <- current
    case t.token of
      TAt -> do
        advance
        case payload of
          Nothing -> pure (Just t.span)
          Just _ -> fatal
            "`@` makes the value the whole token, whose type is the one `%tokentype` named; a `{ ... }` payload type has nothing left to say"
            t.span
      _ -> pure Nothing

  -- One mark says which part of the token the value is and the other says it is
  -- all of it. The `$$` is the later of the two, so it is the one to point at.
  refuseBothMarks whole pattern = case whole, pattern of
    Just _, Just p -> case Array.head p.holes of
      Just hole -> fatal
        "`@` already makes the value the whole token, so there is no part left for `$$` to stand for"
        hole
      Nothing -> pure unit
    _, _ -> pure unit

  displayName fallback = do
    t <- current
    case t.token of
      TString s -> advance $> s
      _ -> pure fallback

-- | `%start { Result } name+`
startDecls :: Decls -> Parser Decls
startDecls acc = do
  resultType <- requiredType "`%start`"
  names <- identifiers1 "a start symbol"
  checkNoneReserved "declared as a start symbol" names
  pure acc
    { starts =
        List.reverse (List.fromFoldable (map (toDecl resultType) names))
          <> acc.starts
    }
  where
  toDecl resultType n = { symbol: n.name, resultType, span: n.span }

-- | `%type { Result } name+`
typeDecls :: Decls -> Parser Decls
typeDecls acc = do
  resultType <- requiredType "`%type`"
  names <- identifiers1 "a nonterminal name"
  checkNoneReserved "given a type" names
  pure acc
    { types =
        List.reverse (List.fromFoldable (map (toDecl resultType) names))
          <> acc.types
    }
  where
  toDecl resultType n = { symbol: n.name, resultType, span: n.span }

-- | `%derive Eq Show`
deriveDecls :: Decls -> Parser Decls
deriveDecls acc = do
  names <- identifiers1 "a class name"
  pure acc { derives = List.reverse (List.fromFoldable names) <> acc.derives }

-- | `%left`, `%right`, `%nonassoc`. Priority comes from the order these appear
-- | in, so they are kept in source order by the assembly step below.
precedenceDecls :: Associativity -> Span -> Decls -> Parser Decls
precedenceDecls associativity span acc = do
  names <- identifiers1 "a token name"
  checkNoneReserved "given a precedence" names
  pure acc
    { precedences =
        { associativity, tokens: map _.name names, span } : acc.precedences
    }

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

rules :: List Rule -> Parser (List Rule)
rules = repeatedly step
  where
  step acc = do
    t <- current
    case t.token of
      TEnd -> pure Nothing
      _ -> do
        r <- rule
        pure (Just (r : acc))

rule :: Parser Rule
rule = do
  start <- current
  inline <- inlineFlag
  head <- identifier "a rule name"
  checkNotReserved head.name head.span "used as a rule name"
  parameters <- ruleParameters
  expect TColon "`:` after the rule name"
  prods <- productions
  end <- optionalSemi
  pure
    { name: head.name
    , parameters
    , inline
    , productions: prods
    , span: { start: start.span.start, end }
    }
  where
  inlineFlag = do
    t <- current
    case t.token of
      TKeyword "inline" -> advance $> true
      _ -> pure false

  optionalSemi = do
    t <- current
    case t.token of
      TSemi -> advance $> t.span.end
      _ -> pure t.span.start

-- | `(A, B)` after a rule name makes it a parameterised rule.
ruleParameters :: Parser (Array String)
ruleParameters = do
  t <- current
  case t.token of
    TLParen -> do
      advance
      names <- commaSeparated (identifier "a parameter name")
      expect TRParen "`)` after the parameter list"
      checkNoneReserved "used as a rule parameter" names
      pure (map _.name names)
    _ -> pure []

commaSeparated :: forall a. Parser a -> Parser (Array a)
commaSeparated item = do
  first <- item
  rest <- repeatedly step Nil
  pure (finish1 first rest)
  where
  step acc = do
    t <- current
    case t.token of
      TComma -> do
        advance
        x <- item
        pure (Just (x : acc))
      _ -> pure Nothing

-- | One or more alternatives. A leading `|` is allowed but not required, which
-- | is what lets every alternative be written with one.
productions :: Parser (Array Production)
productions = do
  leading <- current
  when (leading.token == TBar) advance
  first <- production
  rest <- repeatedly step Nil
  pure (finish1 first rest)
  where
  step acc = do
    t <- current
    case t.token of
      TBar -> do
        advance
        p <- production
        pure (Just (p : acc))
      _ -> pure Nothing

production :: Parser Production
production = do
  start <- current
  elements <- productionElements Nil
  directive <- conflictDirective
  t <- current
  case t.token of
    TBraced braced -> do
      advance
      pure
        { elements
        , directive
        , action: braced.code
        , span: { start: start.span.start, end: t.span.end }
        }
    _ -> fatal
      ("expected a `{ ... }` semantic action, found " <> describe t.token)
      t.span
  where
  -- `%prec TOKEN` or `%shift`, and not both: one asks for a precedence and the
  -- other asks for the production to have none, so a production carrying both
  -- has not said what it wants.
  conflictDirective = do
    t <- current
    case t.token of
      TKeyword "prec" -> do
        advance
        tok <- identifier "a token name after `%prec`"
        checkNotReserved tok.name tok.span "used with `%prec`"
        refuseBoth "shift" "`%prec`"
        pure (Prec tok.name)
      TKeyword "shift" -> do
        advance
        refuseBoth "prec" "`%shift`"
        pure PreferShift
      _ -> pure Inferred

  -- The span is the second one's, not the first's: the first was fine on its
  -- own and the one being read now is the one that cannot be there.
  refuseBoth keyword already = do
    t <- current
    case t.token of
      TKeyword k | k == keyword ->
        fatal
          ( "a production may carry `%prec` or `%shift`, not both; "
              <> already
              <> " is already here"
          )
          t.span
      _ -> pure unit

productionElements :: List Element -> Parser (Array Element)
productionElements seed =
  map (Array.fromFoldable <<< List.reverse) (repeatedly step seed)
  where
  step acc = do
    t <- current
    case t.token of
      TIdent _ -> do
        e <- element
        pure (Just (e : acc))
      _ -> pure Nothing

-- | `binder = symbol` or just `symbol`.
element :: Parser Element
element = do
  start <- current
  binder <- binderName
  symbol <- symbolRef
  end <- current
  pure
    { binder
    , symbol
    , span: { start: start.span.start, end: end.span.start }
    }
  where
  -- A name is a binder only when an `=` follows it; otherwise it is the symbol.
  binderName = Parser \toks i ->
    case Array.index toks i, Array.index toks (i + 1) of
      Just { token: TIdent name }, Just { token: TEquals } ->
        Right { value: Just name, next: i + 2 }
      _, _ -> Right { value: Nothing, next: i }

symbolRef :: Parser SymbolRef
symbolRef = do
  head <- identifier "a symbol name"
  checkNotReserved head.name head.span "referred to in a rule"
  t <- current
  case t.token of
    TLParen -> do
      advance
      args <- commaSeparated symbolRef
      expect TRParen "`)` after the argument list"
      pure (SymbolRef head.name args)
    _ -> pure (SymbolRef head.name [])

--------------------------------------------------------------------------------
-- Entry points
--------------------------------------------------------------------------------

-- | Which mode the grammar is in, settled once every declaration has been read.
-- |
-- | Deciding here rather than while reading is what lets `%tokentype` sit
-- | anywhere among the declarations, like every other one. It is also the last
-- | moment at which the mode can be made structural: after this, a generated
-- | token has no pattern and an external one has nothing else.
tokenSource :: Maybe Code -> Array PendingToken -> Parser TokenSource
tokenSource declared pending = case declared of
  Nothing -> case Array.find (isJust <<< _.whole) pending of
    Just stray -> fatal
      ( "token `" <> stray.decl.name
          <> "` is marked `@`, but the grammar has no `%tokentype`; `@` makes the value the whole token, which is only worth having where the token type is one Puppy did not write"
      )
      (fromMaybe stray.decl.span stray.whole)
    Nothing -> case Array.find (isJust <<< _.pattern) pending of
      Nothing -> pure (GeneratedTokens (map _.decl pending))
      Just stray -> fatal
        ( "token `" <> stray.decl.name
            <> "` is given a pattern, but the grammar has no `%tokentype`; a pattern says how to recognise a value of a token type Puppy did not write"
        )
        (maybe stray.decl.span _.code.span stray.pattern)

  Just tokenType -> do
    tokens <- traverse withPattern pending
    pure (ExternalTokens { tokenType, tokens })
  where
  withPattern p = case p.pattern of
    Just pattern -> pure
      { decl: p.decl
      , pattern
      , value: maybe FromPattern FromToken p.whole
      }
    Nothing -> fatal
      ( "token `" <> p.decl.name
          <> "` needs a `{ ... }` pattern; with `%tokentype` the grammar says how each terminal is recognised, because Puppy is not writing the constructors"
      )
      p.decl.span

grammar :: Parser Grammar
grammar = do
  header <- optionalHeader
  decls <- declarations emptyDecls
  expect TSeparator "`%%` between the declarations and the rules"
  rs <- rules Nil
  expect TEnd "end of file"
  tokens <- tokenSource decls.tokenType
    (Array.fromFoldable (List.reverse decls.tokens))
  pure
    { header
    , tokens
    , starts: Array.fromFoldable (List.reverse decls.starts)
    , types: Array.fromFoldable (List.reverse decls.types)
    , precedences: Array.fromFoldable (List.reverse decls.precedences)
    , derives: Array.fromFoldable (List.reverse decls.derives)
    , rules: Array.fromFoldable (List.reverse rs)
    }
  where
  optionalHeader = do
    t <- current
    case t.token of
      THeader code -> advance $> Just code
      _ -> pure Nothing

-- | Parse an already-lexed grammar.
-- |
-- | The stream has to be one the lexer could have produced, and that takes two
-- | checks rather than one. It has to end with `TEnd`, which is what guarantees
-- | the loops above always meet a token that stops them. And the parse has to
-- | have consumed all of it: a stream carrying a `TEnd` somewhere in the middle
-- | would otherwise finish there and quietly discard everything after it.
parseGrammar :: Array Located -> Either ParseError Grammar
parseGrammar toks = case Array.last toks of
  Just { token: TEnd } -> case unParser grammar toks 0 of
    Left err -> Left err
    Right r
      | r.next == Array.length toks -> Right r.value
      | otherwise -> Left
          { message: "unexpected tokens after the end of the grammar"
          , span: maybe (lastSpan toks) _.span (Array.index toks r.next)
          }
  _ -> Left
    { message: "the token stream does not end with an end-of-file token"
    , span: lastSpan toks
    }

-- | Lex and parse a grammar file.
parse :: String -> Either ParseError Grammar
parse source = case Lexer.lex source of
  Left err -> Left
    { message: err.message
    , span: { start: err.pos, end: err.pos }
    }
  Right toks -> parseGrammar toks
