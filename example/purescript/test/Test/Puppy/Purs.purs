-- | The generated PureScript parser, run.
-- |
-- | Token streams are built by hand here. There is no lexer in this package --
-- | Puppy does not write one and this port is of the grammar -- so the layout
-- | markers a real PureScript lexer would insert are written out as `vopen`,
-- | `vsep` and `vclose`, which is exactly what the grammar sees either way.
module Test.Puppy.Purs where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Puppy.Purs.CST as C
import Puppy.Purs.Parser as P
import Puppy.Purs.Token as T
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

--------------------------------------------------------------------------------
-- Tokens, written out
--------------------------------------------------------------------------------

at :: T.Pos
at = { line: 1, column: 1 }

lo :: String -> T.Token
lo name = T.TokLower { pos: at, name }

up :: String -> T.Token
up name = T.TokUpper { pos: at, name }

qup :: Array String -> String -> T.Token
qup qualifier name = T.TokQualUpper { pos: at, qualifier, name }

opr :: String -> T.Token
opr name = T.TokOperator { pos: at, name }

int :: Int -> T.Token
int value = T.TokInt { pos: at, raw: show value, value }

str :: String -> T.Token
str value = T.TokString { pos: at, raw: show value, value }

lparen :: T.Token
lparen = T.TokLeftParen at

rparen :: T.Token
rparen = T.TokRightParen at

lbrace :: T.Token
lbrace = T.TokLeftBrace at

rbrace :: T.Token
rbrace = T.TokRightBrace at

dcolon :: T.Token
dcolon = T.TokDoubleColon at

rarrow :: T.Token
rarrow = T.TokRightArrow at

larrow :: T.Token
larrow = T.TokLeftArrow at

dot :: T.Token
dot = T.TokDot at

comma :: T.Token
comma = T.TokComma at

equals :: T.Token
equals = T.TokEquals at

pipe :: T.Token
pipe = T.TokPipe at

forAll :: T.Token
forAll = T.TokForall at

vopen :: T.Token
vopen = T.TokLayoutStart at

vsep :: T.Token
vsep = T.TokLayoutSep at

vclose :: T.Token
vclose = T.TokLayoutEnd at

--------------------------------------------------------------------------------
-- The tree, written out
--------------------------------------------------------------------------------

nm :: String -> C.Name
nm name = { pos: at, name }

qn :: String -> C.Qual
qn name = { pos: at, qualifier: [], name }

qqn :: Array String -> String -> C.Qual
qqn qualifier name = { pos: at, qualifier, name }

tvar :: String -> C.Type
tvar = C.TypeVar <<< nm

tcon :: String -> C.Type
tcon = C.TypeConstructor <<< qn

evar :: String -> C.Expr
evar = C.ExprIdent <<< qn

--------------------------------------------------------------------------------

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "types" do
    it "applies a constructor" do
      -- Maybe a
      P.parseType [ up "Maybe", lo "a" ]
        `shouldEqual` Right (C.TypeApp (tcon "Maybe") (tvar "a"))

    it "reads an arrow as right associative" do
      -- a -> b -> c
      P.parseType [ lo "a", rarrow, lo "b", rarrow, lo "c" ]
        `shouldEqual` Right
          (C.TypeArrow (tvar "a") (C.TypeArrow (tvar "b") (tvar "c")))

    it "reads a quantifier" do
      -- forall a. Array a
      P.parseType [ forAll, lo "a", dot, up "Array", lo "a" ]
        `shouldEqual` Right
          ( C.TypeForall [ C.TypeVarName (nm "a") ]
              (C.TypeApp (tcon "Array") (tvar "a"))
          )

    it "reads a record and an open row" do
      -- { x :: Int | r }
      P.parseType [ lbrace, lo "x", dcolon, up "Int", pipe, lo "r", rbrace ]
        `shouldEqual` Right
          ( C.TypeRecord
              (C.Row [ C.RowLabel (nm "x") (tcon "Int") ] (Just (tvar "r")))
          )

    it "reads a qualified constructor" do
      -- Data.Map.Map k v
      P.parseType [ qup [ "Data", "Map" ] "Map", lo "k", lo "v" ]
        `shouldEqual` Right
          ( C.TypeApp
              (C.TypeApp (C.TypeConstructor (qqn [ "Data", "Map" ] "Map")) (tvar "k"))
              (tvar "v")
          )

    it "keeps a constraint apart from an arrow" do
      -- Eq a => a -> a
      P.parseType
        [ up "Eq", lo "a", T.TokRightFatArrow at, lo "a", rarrow, lo "a" ]
        `shouldEqual` Right
          ( C.TypeConstrained (C.TypeApp (tcon "Eq") (tvar "a"))
              (C.TypeArrow (tvar "a") (tvar "a"))
          )

  describe "expressions" do
    it "binds application tighter than an operator" do
      -- f x <> g y
      P.parseExpr [ lo "f", lo "x", opr "<>", lo "g", lo "y" ]
        `shouldEqual` Right
          ( C.ExprOp
              (C.ExprApp (evar "f") (evar "x"))
              (qn "<>")
              (C.ExprApp (evar "g") (evar "y"))
          )

    it "reads a record accessor" do
      -- r.x.y
      P.parseExpr [ lo "r", dot, lo "x", dot, lo "y" ]
        `shouldEqual` Right
          (C.ExprRecordAccessor (evar "r") [ nm "x", nm "y" ])

    it "tells a record update from a record literal" do
      -- r { x = 1 }
      P.parseExpr [ lo "r", lbrace, lo "x", equals, int 1, rbrace ]
        `shouldEqual` Right
          ( C.ExprRecordUpdate (evar "r")
              [ C.RecordUpdateLeaf (nm "x") (C.ExprInt 1) ]
          )

    it "reads a record literal applied to a function" do
      -- f { x: 1 }
      P.parseExpr
        [ lo "f", lbrace, lo "x", T.TokOperator { pos: at, name: ":" }, int 1, rbrace ]
        `shouldEqual` Right
          ( C.ExprApp (evar "f")
              (C.ExprRecord [ C.RecordField (nm "x") (C.ExprInt 1) ])
          )

    it "reads a lambda" do
      -- \x -> x
      P.parseExpr [ T.TokBackslash at, lo "x", rarrow, lo "x" ]
        `shouldEqual` Right
          (C.ExprLambda [ C.BinderVar (nm "x") ] (evar "x"))

  describe "do notation" do
    -- The left of a `<-` is parsed as an expression and turned into a binder
    -- afterwards, because nothing before the arrow says which it is. This is
    -- the part of the port that upstream does with backtracking instead.
    it "reads a bind whose left is a plain name" do
      -- do
      --   x <- f
      --   pure x
      P.parseExpr
        [ lo "do"
        , vopen
        , lo "x"
        , larrow
        , lo "f"
        , vsep
        , lo "pure"
        , lo "x"
        , vclose
        ]
        `shouldEqual` Right
          ( C.ExprDo
              [ C.DoBind (C.BinderVar (nm "x")) (evar "f")
              , C.DoDiscard (C.ExprApp (evar "pure") (evar "x"))
              ]
          )

    it "reads a bind whose left is a constructor pattern" do
      -- do
      --   Tuple a b <- f
      P.parseExpr
        [ lo "do", vopen, up "Tuple", lo "a", lo "b", larrow, lo "f", vclose ]
        `shouldEqual` Right
          ( C.ExprDo
              [ C.DoBind
                  ( C.BinderConstructor (qn "Tuple")
                      [ C.BinderVar (nm "a"), C.BinderVar (nm "b") ]
                  )
                  (evar "f")
              ]
          )

    it "reads a let in a do block" do
      -- do
      --   let y = 1
      --   pure y
      P.parseExpr
        [ lo "do"
        , vopen
        , lo "let"
        , vopen
        , lo "y"
        , equals
        , int 1
        , vclose
        , vsep
        , lo "pure"
        , lo "y"
        , vclose
        ]
        `shouldEqual` Right
          ( C.ExprDo
              [ C.DoLet
                  [ C.LetName (nm "y") []
                      (C.Unconditional (C.Where (C.ExprInt 1) []))
                  ]
              , C.DoDiscard (C.ExprApp (evar "pure") (evar "y"))
              ]
          )

  describe "declarations" do
    it "reads a value binding with arguments" do
      -- add a b = a
      P.parseDecl [ lo "add", lo "a", lo "b", equals, lo "a" ]
        `shouldEqual` Right
          ( C.DeclValue (nm "add")
              [ C.BinderVar (nm "a"), C.BinderVar (nm "b") ]
              (C.Unconditional (C.Where (evar "a") []))
          )

    it "reads a data declaration" do
      -- data Maybe a = Nothing | Just a
      P.parseDecl
        [ lo "data"
        , up "Maybe"
        , lo "a"
        , equals
        , up "Nothing"
        , pipe
        , up "Just"
        , lo "a"
        ]
        `shouldEqual` Right
          ( C.DeclData (C.DataHead (nm "Maybe") [ C.TypeVarName (nm "a") ])
              [ C.DataCtor (nm "Nothing") []
              , C.DataCtor (nm "Just") [ tvar "a" ]
              ]
          )

    it "reads a signature" do
      -- identity :: forall a. a -> a
      P.parseDecl
        [ lo "identity", dcolon, forAll, lo "a", dot, lo "a", rarrow, lo "a" ]
        `shouldEqual` Right
          ( C.DeclSignature (nm "identity")
              (C.TypeForall [ C.TypeVarName (nm "a") ] (C.TypeArrow (tvar "a") (tvar "a")))
          )

    it "reads a class head with a superclass" do
      -- class Eq a <= Ord a
      P.parseDecl
        [ lo "class", up "Eq", lo "a", T.TokLeftFatArrow at, up "Ord", lo "a" ]
        `shouldEqual` Right
          ( C.DeclClass
              ( C.ClassHead
                  { super: [ C.Constraint (qn "Eq") [ tvar "a" ] ]
                  , name: qn "Ord"
                  , vars: [ C.TypeVarName (nm "a") ]
                  , fundeps: []
                  }
              )
              []
          )

  describe "a whole module" do
    it "reads a header, an import and a declaration" do
      -- module Main where
      -- import Prelude
      -- main = 1
      P.parseModule
        [ lo "module"
        , up "Main"
        , lo "where"
        , vopen
        , lo "import"
        , up "Prelude"
        , vsep
        , lo "main"
        , equals
        , int 1
        , vclose
        ]
        `shouldEqual` Right
          ( C.Module (nm "Main") Nothing
              [ C.ImportDecl (qn "Prelude") C.ImportAll Nothing ]
              [ C.DeclValue (nm "main") []
                  (C.Unconditional (C.Where (C.ExprInt 1) []))
              ]
          )

    it "reads an export list" do
      -- module Main (main, class Eq, Maybe(..)) where
      P.parseModule
        [ lo "module"
        , up "Main"
        , lparen
        , lo "main"
        , comma
        , lo "class"
        , up "Eq"
        , comma
        , up "Maybe"
        , T.TokSymbol { pos: at, name: ".." }
        , rparen
        , lo "where"
        , vopen
        , lo "main"
        , equals
        , int 1
        , vclose
        ]
        `shouldEqual` Right
          ( C.Module (nm "Main")
              ( Just
                  [ C.ExportValue (nm "main")
                  , C.ExportClass (nm "Eq")
                  , C.ExportType (nm "Maybe") (Just C.DataAll)
                  ]
              )
              []
              [ C.DeclValue (nm "main") []
                  (C.Unconditional (C.Where (C.ExprInt 1) []))
              ]
          )

  describe "errors" do
    it "names what could have come next" do
      case P.parseType [ up "Maybe", rarrow, rarrow ] of
        Right t -> fail ("expected a parse error, got " <> show t)
        Left err -> err.found `shouldEqual` Just (T.TokRightArrow at)

    it "counts the tokens it read before stopping" do
      case P.parseExpr [ lo "f", lo "x", equals ] of
        Right e -> fail ("expected a parse error, got " <> show e)
        Left err -> err.position `shouldEqual` 2
  where
  fail message = message `shouldEqual` ""
