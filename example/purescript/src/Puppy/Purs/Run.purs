-- | Source text in, tree out, with somebody else's lexer in between.
-- |
-- | `purescript-language-cst-parser` has a lexer and an offside rule for this
-- | language already, and they are the two things Puppy does not write. Its
-- | `lex` hands back a lazy stream of `SourceToken` with the block markers
-- | already inserted -- which is exactly the shape a pulling entry point wants,
-- | one token at a time and nothing read ahead.
-- |
-- | So nothing here transduces anything. The layout pass that
-- | `Puppy.Runtime.Source` exists for has already happened, on the other side
-- | of `lex`.
module Puppy.Purs.Run
  ( LexFailure
  , Parsed
  , parseModule
  , parseModuleRecovering
  , parseExpr
  , parseType
  , parseDecl
  ) where

import Prelude

import Control.Monad.State (StateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Puppy.Purs.CST as C
import Puppy.Purs.Parser as P
import Puppy.Runtime (ParseError, RecoveryResult)
import PureScript.CST.Errors (ParseError) as CST
import PureScript.CST.Lexer (lex, lexModule)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (SourcePos, SourceToken)

-- | The lexer gave up before the parser had a chance to.
type LexFailure = { position :: SourcePos, error :: CST.ParseError }

-- | Two ways to fail and one to succeed. The outer `Either` is the lexer's and
-- | the inner one is the parser's, which is what keeps a character nobody can
-- | read from being reported as a token nobody expected.
type Parsed a = Either LexFailure (Either (ParseError SourceToken) a)

type Lexing = StateT TokenStream (Either LexFailure)

-- | One token, and the rest of the stream to go on with.
next :: Lexing (Maybe SourceToken)
next = do
  stream <- State.get
  case TokenStream.step stream of
    TokenEOF _ _ -> pure Nothing
    TokenError position error _ _ -> lift (Left { position, error })
    TokenCons token _ rest _ -> do
      State.put rest
      pure (Just token)

runWith
  :: forall a
   . (String -> TokenStream)
  -> (Lexing (Maybe SourceToken) -> Lexing a)
  -> String
  -> Either LexFailure a
runWith lexer entry source = State.evalStateT (entry next) (lexer source)

-- | A whole module, which may open with a shebang line.
-- |
-- | `lexModule` is `lex` with that allowed for, and it is the one thing about
-- | a module that is not simply "the same tokens, starting from a different
-- | rule". A fragment cannot have one, so the others use `lex`.
parseModule :: String -> Parsed C.Module
parseModule = runWith lexModule P.parseModuleFrom

-- | The same, carrying on past what it can.
-- |
-- | The lexer is still allowed to give up -- a character nobody can read is
-- | not a syntax error and there is nothing for the grammar to recover from --
-- | so the outer `Either` stays. What changes is the inner answer: a tree with
-- | holes in it, and the errors that put them there.
parseModuleRecovering
  :: String -> Either LexFailure (RecoveryResult SourceToken C.Module)
parseModuleRecovering =
  runWith lexModule P.parseModuleRecoveringFrom

parseExpr :: String -> Parsed C.Expr
parseExpr = runWith lex P.parseExprFrom

parseType :: String -> Parsed C.Type
parseType = runWith lex P.parseTypeFrom

parseDecl :: String -> Parsed C.Decl
parseDecl = runWith lex P.parseDeclFrom
