-- | The generated PureScript parser, run over source text.
-- |
-- | The tokens come from `purescript-language-cst-parser`'s lexer, offside rule
-- | and all, so what is being checked here is the grammar and the tree it
-- | builds -- with the two things Puppy does not do borrowed from someone who
-- | does them.
module Test.Puppy.Purs where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Puppy.Purs.CST as C
import Data.Array as Array
import Puppy.Purs.Run as Run
import Puppy.Runtime (RecoveryResult(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | What a parse produced, with both ways of failing flattened into a message
-- | so that a test can say what it wanted and see what it got.
type Result a = Either String a

typeOf :: String -> Result C.Type
typeOf = flatten <<< Run.parseType

exprOf :: String -> Result C.Expr
exprOf = flatten <<< Run.parseExpr

declOf :: String -> Result C.Decl
declOf = flatten <<< Run.parseDecl

moduleOf :: String -> Result C.Module
moduleOf = flatten <<< Run.parseModule

flatten :: forall a. Run.Parsed a -> Result a
flatten = case _ of
  Left lexError -> Left ("lex error at line " <> show lexError.position.line)
  Right (Left parseError) -> Left
    ( "parse error at token " <> show parseError.position
        <> ", expected "
        <> show parseError.expected
    )
  Right (Right value) -> Right value

nm :: Int -> Int -> String -> C.Name
nm line column name = { pos: { line, column }, name }

qn :: Int -> Int -> String -> C.Qual
qn line column name = { pos: { line, column }, qualifier: Nothing, name }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "types" do
    -- Positions come from the lexer, so they are the real ones. Lines and
    -- columns are zero-based here because that is what `SourcePos` is.
    it "applies a constructor" do
      typeOf "Maybe a" `shouldEqual` Right
        ( C.TypeApp (C.TypeConstructor (qn 0 0 "Maybe"))
            (C.TypeVar (nm 0 6 "a"))
        )

    it "reads an arrow as right associative" do
      typeOf "a -> b -> c" `shouldEqual` Right
        ( C.TypeArrow (C.TypeVar (nm 0 0 "a"))
            (C.TypeArrow (C.TypeVar (nm 0 5 "b")) (C.TypeVar (nm 0 10 "c")))
        )

    it "reads a quantifier" do
      typeOf "forall a. Array a" `shouldEqual` Right
        ( C.TypeForall [ C.TypeVarName (nm 0 7 "a") ]
            (C.TypeApp (C.TypeConstructor (qn 0 10 "Array")) (C.TypeVar (nm 0 16 "a")))
        )

    it "reads a record and an open row" do
      typeOf "{ x :: Int | r }" `shouldEqual` Right
        ( C.TypeRecord
            ( C.Row [ C.RowLabel (nm 0 2 "x") (C.TypeConstructor (qn 0 7 "Int")) ]
                (Just (C.TypeVar (nm 0 13 "r")))
            )
        )

    it "keeps a qualifier the lexer read" do
      typeOf "Data.Map.Map k v" `shouldEqual` Right
        ( C.TypeApp
            ( C.TypeApp
                ( C.TypeConstructor
                    { pos: { line: 0, column: 0 }
                    , qualifier: Just "Data.Map"
                    , name: "Map"
                    }
                )
                (C.TypeVar (nm 0 13 "k"))
            )
            (C.TypeVar (nm 0 15 "v"))
        )

    it "keeps a constraint apart from an arrow" do
      typeOf "Eq a => a -> a" `shouldEqual` Right
        ( C.TypeConstrained
            (C.TypeApp (C.TypeConstructor (qn 0 0 "Eq")) (C.TypeVar (nm 0 3 "a")))
            (C.TypeArrow (C.TypeVar (nm 0 8 "a")) (C.TypeVar (nm 0 13 "a")))
        )

  describe "expressions" do
    it "binds application tighter than an operator" do
      exprOf "f x <> g y" `shouldEqual` Right
        ( C.ExprOp
            (C.ExprApp (C.ExprIdent (qn 0 0 "f")) (C.ExprIdent (qn 0 2 "x")))
            (qn 0 4 "<>")
            (C.ExprApp (C.ExprIdent (qn 0 7 "g")) (C.ExprIdent (qn 0 9 "y")))
        )

    it "reads a record accessor" do
      exprOf "r.x.y" `shouldEqual` Right
        (C.ExprRecordAccessor (C.ExprIdent (qn 0 0 "r")) [ nm 0 2 "x", nm 0 4 "y" ])

    it "tells a record update from a record literal" do
      exprOf "r { x = 1 }" `shouldEqual` Right
        ( C.ExprRecordUpdate (C.ExprIdent (qn 0 0 "r"))
            [ C.RecordUpdateLeaf (nm 0 4 "x") (C.ExprInt 1) ]
        )

    it "reads a record literal applied to a function" do
      exprOf "f { x: 1 }" `shouldEqual` Right
        ( C.ExprApp (C.ExprIdent (qn 0 0 "f"))
            (C.ExprRecord [ C.RecordField (nm 0 4 "x") (C.ExprInt 1) ])
        )

    it "reads a keyword used as a name, with its position" do
      -- `as` is a name here and a keyword elsewhere, and `@` is what keeps the
      -- position it was written at.
      exprOf "as" `shouldEqual` Right (C.ExprIdent (qn 0 0 "as"))

  describe "do notation, over the real offside rule" do
    it "reads a bind whose left is a plain name" do
      exprOf "do\n  x <- f\n  pure x" `shouldEqual` Right
        ( C.ExprDo
            [ C.DoBind (C.BinderVar (nm 1 2 "x")) (C.ExprIdent (qn 1 7 "f"))
            , C.DoDiscard
                (C.ExprApp (C.ExprIdent (qn 2 2 "pure")) (C.ExprIdent (qn 2 7 "x")))
            ]
        )

    it "reads a bind whose left is a constructor pattern" do
      exprOf "do\n  Tuple a b <- f" `shouldEqual` Right
        ( C.ExprDo
            [ C.DoBind
                ( C.BinderConstructor (qn 1 2 "Tuple")
                    [ C.BinderVar (nm 1 8 "a"), C.BinderVar (nm 1 10 "b") ]
                )
                (C.ExprIdent (qn 1 15 "f"))
            ]
        )

    it "reads a let block inside a do block" do
      exprOf "do\n  let y = 1\n  pure y" `shouldEqual` Right
        ( C.ExprDo
            [ C.DoLet
                [ C.LetName (nm 1 6 "y") []
                    (C.Unconditional (C.Where (C.ExprInt 1) []))
                ]
            , C.DoDiscard
                (C.ExprApp (C.ExprIdent (qn 2 2 "pure")) (C.ExprIdent (qn 2 7 "y")))
            ]
        )

  describe "declarations" do
    it "reads a value binding with arguments" do
      declOf "add a b = a" `shouldEqual` Right
        ( C.DeclValue (nm 0 0 "add")
            [ C.BinderVar (nm 0 4 "a"), C.BinderVar (nm 0 6 "b") ]
            (C.Unconditional (C.Where (C.ExprIdent (qn 0 10 "a")) []))
        )

    it "reads a data declaration" do
      declOf "data Maybe a = Nothing | Just a" `shouldEqual` Right
        ( C.DeclData (C.DataHead (nm 0 5 "Maybe") [ C.TypeVarName (nm 0 11 "a") ])
            [ C.DataCtor (nm 0 15 "Nothing") []
            , C.DataCtor (nm 0 25 "Just") [ C.TypeVar (nm 0 30 "a") ]
            ]
        )

    it "reads a class head with a superclass" do
      declOf "class Eq a <= Ord a" `shouldEqual` Right
        ( C.DeclClass
            ( C.ClassHead
                { super: [ C.Constraint (qn 0 6 "Eq") [ C.TypeVar (nm 0 9 "a") ] ]
                , name: qn 0 14 "Ord"
                , vars: [ C.TypeVarName (nm 0 18 "a") ]
                , fundeps: []
                }
            )
            []
        )

  describe "whole modules" do
    it "reads a header, an import and a declaration" do
      moduleOf "module Main where\nimport Prelude\nmain = 1\n" `shouldEqual` Right
        ( C.Module (nm 0 7 "Main") Nothing
            [ C.ImportDecl (qn 1 7 "Prelude") C.ImportAll Nothing ]
            [ C.DeclValue (nm 2 0 "main") []
                (C.Unconditional (C.Where (C.ExprInt 1) []))
            ]
        )

    -- A module may open with a shebang, and `lexModule` is what allows for it.
    -- Every other entry point takes a fragment, which cannot have one.
    it "reads a module that opens with a shebang" do
      moduleOf "#!/usr/bin/env purs\nmodule Main where\nmain = 1\n" `shouldEqual` Right
        ( C.Module (nm 1 7 "Main") Nothing []
            [ C.DeclValue (nm 2 0 "main") []
                (C.Unconditional (C.Where (C.ExprInt 1) []))
            ]
        )

  describe "carrying on past an error" do
    -- A declaration nobody can read ends where the next one begins. Everything
    -- around it is still read, which is what an editor wants and what the
    -- entry points that stop at the first error cannot give.
    -- The hole stays in the tree, where the declaration was. A module that
    -- simply had one declaration fewer would read as though the file were
    -- fine, and the position is what a later pass has to put a marker on.
    it "reads the declarations either side of a broken one" do
      case Run.parseModuleRecovering "module M where\ngood = 2\n= 1\nalso = 3\n" of
        Left _ -> fail "expected the lexer to manage"
        Right (ParseSucceeded _) -> fail "expected something to be wrong"
        Right (ParseFailed _) -> fail "expected it to carry on"
        Right (ParseRecovered errors (C.Module _ _ _ decls)) -> do
          Array.length errors `shouldEqual` 1
          map declName decls `shouldEqual` [ "good", "<broken 2:0>", "also" ]

    -- Three places say a parse may be picked up again, and each of them is a
    -- list the layout pass has already marked the boundaries of. What is
    -- checked is both halves: that the broken one became a hole, and that the
    -- good one after it is still there and still whole.
    it "carries on inside a `do` block" do
      case Run.parseModuleRecovering "module M where\nmain = do\n  = 1\n  pure 2\n" of
        Right (ParseRecovered errors m) -> do
          Array.length errors `shouldEqual` 1
          doStatements m `shouldEqual`
            [ C.DoBroken (Just { line: 2, column: 2 })
            , C.DoDiscard
                (C.ExprApp (C.ExprIdent (qn 3 2 "pure")) (C.ExprInt 2))
            ]
        _ -> fail "expected a recovered parse"

    it "carries on inside a `let` block" do
      case
        Run.parseModuleRecovering
          "module M where\nmain =\n  let\n    = 1\n    y = 2\n  in y\n"
        of
        Right (ParseRecovered errors m) -> do
          Array.length errors `shouldEqual` 1
          letBindings m `shouldEqual`
            [ C.LetBroken (Just { line: 3, column: 4 })
            , C.LetName (nm 4 4 "y") []
                (C.Unconditional (C.Where (C.ExprInt 2) []))
            ]
        _ -> fail "expected a recovered parse"

    it "reports every broken declaration, not only the first" do
      case
        Run.parseModuleRecovering
          "module M where\n= 1\ngood = 2\n= 3\nalso = 4\n"
        of
        Right (ParseRecovered errors (C.Module _ _ _ decls)) -> do
          Array.length errors `shouldEqual` 2
          map declName decls `shouldEqual`
            [ "<broken 1:0>", "good", "<broken 3:0>", "also" ]
        _ -> fail "expected a recovered parse"

    it "says so plainly when nothing is wrong" do
      case Run.parseModuleRecovering "module M where\nmain = 1\n" of
        Right (ParseSucceeded (C.Module _ _ _ decls)) ->
          map declName decls `shouldEqual` [ "main" ]
        _ -> fail "expected a clean parse"

  describe "failures" do
    it "reports a syntax error against the token the parser stopped at" do
      case typeOf "Maybe -> ->" of
        Right t -> fail ("expected a parse error, got " <> show t)
        Left message -> (message /= "") `shouldEqual` true

    it "keeps a character the lexer cannot read out of the parser's business" do
      case exprOf "1 \x00 2" of
        Right e -> fail ("expected a lex error, got " <> show e)
        Left message -> (message /= "") `shouldEqual` true

-- | The name a declaration binds, for saying which ones survived -- and, for
-- | one that could not be read, where it was, so that the hole is pinned to a
-- | position and not merely counted.
declName :: C.Decl -> String
declName = case _ of
  C.DeclValue n _ _ -> n.name
  C.DeclSignature n _ -> n.name
  C.DeclBroken (Just p) -> "<broken " <> show p.line <> ":" <> show p.column <> ">"
  C.DeclBroken Nothing -> "<broken>"
  _ -> "?"

-- | The statements of the `do` block a module's only declaration is.
doStatements :: C.Module -> Array C.DoStatement
doStatements = case _ of
  C.Module _ _ _ [ C.DeclValue _ _ (C.Unconditional (C.Where (C.ExprDo ss) _)) ] ->
    ss
  _ -> []

-- | The bindings of the `let` block a module's only declaration is.
letBindings :: C.Module -> Array C.LetBinding
letBindings = case _ of
  C.Module _ _ _ [ C.DeclValue _ _ (C.Unconditional (C.Where (C.ExprLet bs _) _)) ] ->
    bs
  _ -> []
