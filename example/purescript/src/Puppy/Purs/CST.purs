-- | The tree the grammar builds.
-- |
-- | Smaller than the compiler's own CST, and deliberately. That one keeps every
-- | token it read -- every paren, comma and layout marker -- because a
-- | formatter has to put the source back exactly as it found it. Nothing here
-- | is going to print anything, so what is kept is the shape and the names,
-- | and punctuation that only held things apart is dropped.
-- |
-- | The functions at the bottom are the other half of the port. Upstream runs
-- | its semantic actions in a `Parser` monad and several of them fail there --
-- | a name that is not allowed, a record update where a record literal should
-- | be. Puppy's actions are ordinary PureScript expressions with nowhere to
-- | fail to, so a check that upstream makes while parsing is either recorded in
-- | the tree for a later pass to find, or made a pure reinterpretation of what
-- | was parsed. Both kinds are here rather than in the grammar, so that the
-- | grammar stays a grammar.
module Puppy.Purs.CST where

import Prelude

-- `Type`, `Row` and `Constraint` are all kinds in `Prim`, which is imported
-- implicitly. The grammar names three of its own after them, so the ones from
-- `Prim` are hidden rather than shadowed.
import Prim hiding (Constraint, Row, Type)

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.CodeUnits as SCU
import Puppy.Runtime (ParseError)
import PureScript.CST.Types as T

-- | A name and where it was written.
-- | A name, and where it was written.
-- |
-- | Every terminal in the grammar is marked `@`, so every one of them hands
-- | its rule the `SourceToken` the lexer made -- and everything below is a way
-- | of reading one. Nothing is rebuilt from a constant and nothing loses its
-- | position, which is the difference `@` makes: a pattern that pins a keyword
-- | down still hands over the token it pinned.
type Name = { pos :: T.SourcePos, name :: String }

-- | A name that may carry a module qualifier.
-- | The qualifier is unwrapped to its text. `ModuleName` is a newtype without a
-- | `Show`, and this tree derives one for every node so that a failing test can
-- | say what it got.
type Qual = { pos :: T.SourcePos, qualifier :: Maybe String, name :: String }

-- | The text a token stands for, whatever kind of token it is.
-- |
-- | A grammar only asks this of terminals whose pattern already said which
-- | kind they are, so the last arm is for a token no rule can be holding.
textOf :: T.SourceToken -> String
textOf t = case t.value of
  T.TokLowerName _ s -> s
  T.TokUpperName _ s -> s
  T.TokSymbolName _ s -> s
  T.TokOperator _ s -> s
  T.TokHole s -> s
  T.TokString _ s -> s
  T.TokRawString s -> s
  T.TokSymbolArrow _ -> "(->)"
  _ -> ""

qualifierOf :: T.SourceToken -> Maybe String
qualifierOf t = map (\(T.ModuleName m) -> m) case t.value of
  T.TokLowerName q _ -> q
  T.TokUpperName q _ -> q
  T.TokSymbolName q _ -> q
  T.TokOperator q _ -> q
  _ -> Nothing

name :: T.SourceToken -> Name
name t = { pos: t.range.start, name: textOf t }

qualName :: T.SourceToken -> Qual
qualName t =
  { pos: t.range.start, qualifier: qualifierOf t, name: textOf t }

-- | A qualified name with its qualifier dropped, for the places that take a
-- | plain one -- a module's own name, or the name it is imported as.
plain :: Qual -> Name
plain q = { pos: q.pos, name: q.name }

-- | An operator, which is a name the grammar reads the same way.
operator :: T.SourceToken -> Name
operator = name

qualOperator :: T.SourceToken -> Qual
qualOperator = qualName

intValue :: T.SourceToken -> Int
intValue t = case t.value of
  T.TokInt _ (T.SmallInt n) -> n
  _ -> 0

numberValue :: T.SourceToken -> Number
numberValue t = case t.value of
  T.TokNumber _ n -> n
  _ -> 0.0

charValue :: T.SourceToken -> Char
charValue t = case t.value of
  T.TokChar _ c -> c
  _ -> '?'

stringValue :: T.SourceToken -> String
stringValue = textOf

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Type
  = TypeVar Name
  | TypeConstructor Qual
  | TypeOpName Qual
  | TypeHole Name
  | TypeWildcard
  | TypeArrowName
  | TypeString String
  | TypeInt Int
  | TypeRecord Row
  | TypeRow Row
  | TypeParens Type
  | TypeKinded Type Type
  | TypeApp Type Type
  | TypeOp Type Qual Type
  | TypeArrow Type Type
  | TypeConstrained Type Type
  | TypeForall (Array TypeVarBinding) Type

data TypeVarBinding
  = TypeVarName Name
  | TypeVarKinded Name Type

data Row = Row (Array RowLabel) (Maybe Type)

data RowLabel = RowLabel Name Type

derive instance Eq Type
derive instance Generic Type _
instance Show Type where
  show x = genericShow x

derive instance Eq TypeVarBinding
derive instance Generic TypeVarBinding _
instance Show TypeVarBinding where
  show x = genericShow x

derive instance Eq Row
derive instance Generic Row _
instance Show Row where
  show x = genericShow x

derive instance Eq RowLabel
derive instance Generic RowLabel _
instance Show RowLabel where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data Expr
  = ExprSection
  | ExprHole Name
  | ExprIdent Qual
  | ExprConstructor Qual
  | ExprOpName Qual
  | ExprBoolean Boolean
  | ExprChar Char
  | ExprString String
  | ExprInt Int
  | ExprNumber Number
  | ExprArray (Array Expr)
  | ExprRecord (Array (RecordLabeled Expr))
  | ExprParens Expr
  | ExprTyped Expr Type
  | ExprOp Expr Qual Expr
  | ExprInfix Expr Expr Expr
  | ExprNegate Expr
  | ExprApp Expr Expr
  | ExprIf Expr Expr Expr
  | ExprDo (Array DoStatement)
  | ExprAdo (Array DoStatement) Expr
  | ExprLambda (Array Binder) Expr
  | ExprLet (Array LetBinding) Expr
  | ExprCase (Array Expr) (Array CaseBranch)
  | ExprRecordAccessor Expr (Array Name)
  | ExprRecordUpdate Expr (Array RecordUpdate)
  -- ^ Something the grammar accepted and a later pass has to complain about.
  | ExprError String Expr

data RecordLabeled a
  = RecordPun Name
  | RecordField Name a

data RecordUpdate
  = RecordUpdateLeaf Name Expr
  | RecordUpdateBranch Name (Array RecordUpdate)

-- | Inside `x { ... }` a field may be any of three things, and which of them
-- | decides whether the whole is a record literal applied to `x` or an update
-- | of it. Upstream sorts that out in a monadic action; here the alternatives
-- | are a type and `recordApplyOrUpdate` sorts them.
data RecordUpdateOrLabel
  = UpdateOrLabelPun Name
  | UpdateOrLabelField Name Expr
  | UpdateOrLabelLeaf Name Expr
  | UpdateOrLabelBranch Name (Array RecordUpdate)

data Where = Where Expr (Array LetBinding)

data LetBinding
  = LetBroken (Maybe T.SourcePos)
  | LetSignature Name Type
  | LetName Name (Array Binder) Guarded
  | LetPattern Binder Guarded

data Guarded
  = Unconditional Where
  | Guarded (Array GuardedExpr)

data GuardedExpr = GuardedExpr (Array PatternGuard) Where

data PatternGuard
  = GuardExpr Expr
  | GuardBind Binder Expr

data DoStatement
  = DoBroken (Maybe T.SourcePos)
  | DoLet (Array LetBinding)
  | DoDiscard Expr
  | DoBind Binder Expr

data CaseBranch = CaseBranch (Array Binder) Guarded

derive instance Eq Expr
derive instance Generic Expr _
instance Show Expr where
  show x = genericShow x

derive instance Eq a => Eq (RecordLabeled a)
derive instance Generic (RecordLabeled a) _
instance Show a => Show (RecordLabeled a) where
  show x = genericShow x

derive instance Eq RecordUpdate
derive instance Generic RecordUpdate _
instance Show RecordUpdate where
  show x = genericShow x

derive instance Eq RecordUpdateOrLabel
derive instance Generic RecordUpdateOrLabel _
instance Show RecordUpdateOrLabel where
  show x = genericShow x

derive instance Eq Where
derive instance Generic Where _
instance Show Where where
  show x = genericShow x

derive instance Eq LetBinding
derive instance Generic LetBinding _
instance Show LetBinding where
  show x = genericShow x

derive instance Eq Guarded
derive instance Generic Guarded _
instance Show Guarded where
  show x = genericShow x

derive instance Eq GuardedExpr
derive instance Generic GuardedExpr _
instance Show GuardedExpr where
  show x = genericShow x

derive instance Eq PatternGuard
derive instance Generic PatternGuard _
instance Show PatternGuard where
  show x = genericShow x

derive instance Eq DoStatement
derive instance Generic DoStatement _
instance Show DoStatement where
  show x = genericShow x

derive instance Eq CaseBranch
derive instance Generic CaseBranch _
instance Show CaseBranch where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Binders
--------------------------------------------------------------------------------

data Binder
  = BinderWildcard
  | BinderVar Name
  | BinderNamed Name Binder
  | BinderConstructor Qual (Array Binder)
  | BinderBoolean Boolean
  | BinderChar Char
  | BinderString String
  | BinderInt Int
  | BinderNumber Number
  | BinderArray (Array Binder)
  | BinderRecord (Array (RecordLabeled Binder))
  | BinderParens Binder
  | BinderTyped Binder Type
  | BinderOp Binder Qual Binder
  | BinderError String

derive instance Eq Binder
derive instance Generic Binder _
instance Show Binder where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

data Module = Module Name (Maybe (Array Export)) (Array ImportDecl) (Array Decl)

data Export
  = ExportValue Name
  | ExportOp Name
  | ExportType Name (Maybe DataMembers)
  | ExportTypeOp Name
  | ExportClass Name
  | ExportModule Qual

data DataMembers
  = DataAll
  | DataEnumerated (Array Name)

data ImportDecl = ImportDecl Qual ImportList (Maybe Name)

data ImportList
  = ImportAll
  | ImportOnly (Array Import)
  | ImportHiding (Array Import)

data Import
  = ImportValue Name
  | ImportOp Name
  | ImportType Name (Maybe DataMembers)
  | ImportTypeOp Name
  | ImportClass Name

-- | A run of declarations joined by `else`, or a single import. What
-- | `moduleDecls` is a list of.
data DeclChain
  = ChainImport ImportDecl
  | ChainDecls (Array Decl)
  | ChainBroken (Maybe T.SourcePos)

data KindKeyword = KindData | KindNewtype | KindType | KindClass

data Decl
  = DeclBroken (Maybe T.SourcePos)
  | DeclData DataHead (Array DataCtor)
  | DeclType DataHead Type
  | DeclNewtype DataHead Name Type
  | DeclKindSignature KindKeyword Name Type
  | DeclClass ClassHead (Array Labeled)
  | DeclInstance InstanceHead (Array InstanceBinding)
  | DeclDerive Boolean InstanceHead
  | DeclSignature Name Type
  | DeclValue Name (Array Binder) Guarded
  | DeclFixity Fixity
  | DeclForeignValue Name Type
  | DeclForeignData Name Type
  | DeclRole Name (Array Role)

data DataHead = DataHead Name (Array TypeVarBinding)

data DataCtor = DataCtor Name (Array Type)

data Labeled = Labeled Name Type

data ClassHead = ClassHead
  { super :: Array Constraint
  , name :: Qual
  , vars :: Array TypeVarBinding
  , fundeps :: Array Fundep
  }

data Constraint = Constraint Qual (Array Type)

data Fundep
  = FundepDetermined (Array Name)
  | FundepDetermines (Array Name) (Array Name)

data InstanceHead = InstanceHead (Maybe Name) (Array Constraint) Qual (Array Type)

data InstanceBinding
  = InstanceSignature Name Type
  | InstanceName Name (Array Binder) Guarded

data FixityKeyword = Infix | Infixl | Infixr

data Fixity
  = FixityValue FixityKeyword Int Qual Name
  | FixityType FixityKeyword Int Qual Name

data Role = RoleNominal | RoleRepresentational | RolePhantom

derive instance Eq Module
derive instance Generic Module _
instance Show Module where
  show x = genericShow x

derive instance Eq Export
derive instance Generic Export _
instance Show Export where
  show x = genericShow x

derive instance Eq DataMembers
derive instance Generic DataMembers _
instance Show DataMembers where
  show x = genericShow x

derive instance Eq ImportDecl
derive instance Generic ImportDecl _
instance Show ImportDecl where
  show x = genericShow x

derive instance Eq ImportList
derive instance Generic ImportList _
instance Show ImportList where
  show x = genericShow x

derive instance Eq Import
derive instance Generic Import _
instance Show Import where
  show x = genericShow x

derive instance Eq DeclChain
derive instance Generic DeclChain _
instance Show DeclChain where
  show x = genericShow x

derive instance Eq KindKeyword
derive instance Generic KindKeyword _
instance Show KindKeyword where
  show x = genericShow x

derive instance Eq Decl
derive instance Generic Decl _
instance Show Decl where
  show x = genericShow x

derive instance Eq DataHead
derive instance Generic DataHead _
instance Show DataHead where
  show x = genericShow x

derive instance Eq DataCtor
derive instance Generic DataCtor _
instance Show DataCtor where
  show x = genericShow x

derive instance Eq Labeled
derive instance Generic Labeled _
instance Show Labeled where
  show x = genericShow x

derive instance Eq ClassHead
derive instance Generic ClassHead _
instance Show ClassHead where
  show x = genericShow x

derive instance Eq Constraint
derive instance Generic Constraint _
instance Show Constraint where
  show x = genericShow x

derive instance Eq Fundep
derive instance Generic Fundep _
instance Show Fundep where
  show x = genericShow x

derive instance Eq InstanceHead
derive instance Generic InstanceHead _
instance Show InstanceHead where
  show x = genericShow x

derive instance Eq InstanceBinding
derive instance Generic InstanceBinding _
instance Show InstanceBinding where
  show x = genericShow x

derive instance Eq FixityKeyword
derive instance Generic FixityKeyword _
instance Show FixityKeyword where
  show x = genericShow x

derive instance Eq Fixity
derive instance Generic Fixity _
instance Show Fixity where
  show x = genericShow x

derive instance Eq Role
derive instance Generic Role _
instance Show Role where
  show x = genericShow x

--------------------------------------------------------------------------------
-- What the grammar could not decide on its own
--------------------------------------------------------------------------------

-- | Record application, associated the way `expr4 expr5` leaves it.
-- |
-- | Kept from `Parser.y`: a record update or literal on the right of an
-- | application arrives already applied, so the application has to be
-- | re-associated to the left.
exprApp :: Expr -> Expr -> Expr
exprApp f a = case a of
  ExprApp lhs rhs -> ExprApp (ExprApp f lhs) rhs
  _ -> ExprApp f a

-- | `x { ... }` is a record literal applied to `x` if every field is `label:
-- | value` or a pun, and an update of `x` if every one is `label = value` or a
-- | nested `label { ... }`. A mixture is neither.
recordApplyOrUpdate :: Expr -> Array RecordUpdateOrLabel -> Expr
recordApplyOrUpdate subject fields =
  if Array.all isLabel fields then
    ExprApp subject (ExprRecord (map asLabel fields))
  else if Array.all isUpdate fields then
    ExprRecordUpdate subject (map asUpdate fields)
  else
    ExprError "a record cannot mix `:` fields with `=` updates" subject
  where
  isLabel = case _ of
    UpdateOrLabelPun _ -> true
    UpdateOrLabelField _ _ -> true
    _ -> false

  isUpdate = case _ of
    UpdateOrLabelLeaf _ _ -> true
    UpdateOrLabelBranch _ _ -> true
    _ -> false

  asLabel = case _ of
    UpdateOrLabelPun l -> RecordPun l
    UpdateOrLabelField l e -> RecordField l e
    UpdateOrLabelLeaf l e -> RecordField l e
    UpdateOrLabelBranch l _ -> RecordPun l

  asUpdate = case _ of
    UpdateOrLabelLeaf l e -> RecordUpdateLeaf l e
    UpdateOrLabelBranch l us -> RecordUpdateBranch l us
    UpdateOrLabelPun l -> RecordUpdateLeaf l (ExprIdent { pos: l.pos, qualifier: Nothing, name: l.name })
    UpdateOrLabelField l e -> RecordUpdateLeaf l e

-- | `{ label = value }` in a record literal. Upstream reports it while parsing;
-- | here the tree carries the complaint.
recordUpdateInCtr :: Name -> Expr -> RecordLabeled Expr
recordUpdateInCtr l e =
  RecordField l (ExprError "use `:` to give a field a value, not `=`" e)

recordBinderInCtr :: Name -> Binder -> RecordLabeled Binder
recordBinderInCtr l _ =
  RecordField l (BinderError "use `:` to match a field, not `=`")

-- | A run of binder atoms is a constructor pattern if the first of them is a
-- | constructor, and is otherwise only allowed to be one atom long.
binderConstructor :: Array Binder -> Binder
binderConstructor atoms = case Array.uncons atoms of
  Nothing -> BinderError "empty binder"
  Just { head, tail }
    | Array.null tail -> head
    | otherwise -> case head of
        BinderConstructor n [] -> BinderConstructor n tail
        _ -> BinderError "only a constructor can be applied in a pattern"

-- | An expression read as a binder.
-- |
-- | This is how `do` statements and pattern guards are parsed here. `Foo a b`
-- | is a constructor pattern or three applications and nothing before the `<-`
-- | says which, so the left of a `<-` is parsed as an expression and converted
-- | once it is known to be a binder. Upstream's comment describes this exact
-- | approach and declines it, on the grounds that a hand-written reassociation
-- | is hard to audit; it takes happy's backtracking instead, which Puppy has
-- | no equivalent of.
toBinder :: Expr -> Binder
toBinder = case _ of
  ExprSection -> BinderWildcard
  ExprIdent q
    | isNothing q.qualifier -> BinderVar { pos: q.pos, name: q.name }
  ExprConstructor q -> BinderConstructor q []
  ExprBoolean b -> BinderBoolean b
  ExprChar c -> BinderChar c
  ExprString s -> BinderString s
  ExprInt i -> BinderInt i
  ExprNumber d -> BinderNumber d
  ExprNegate (ExprInt i) -> BinderInt (negate i)
  ExprNegate (ExprNumber d) -> BinderNumber (negate d)
  ExprArray es -> BinderArray (map toBinder es)
  ExprRecord fs -> BinderRecord (map (mapLabeled toBinder) fs)
  ExprParens e -> BinderParens (toBinder e)
  ExprTyped e t -> BinderTyped (toBinder e) t
  -- `x@p` reaches here as an application of the `@` operator, which is what
  -- the lexer makes of it outside a pattern.
  ExprOp lhs op rhs
    | isNothing op.qualifier && op.name == "@" -> case lhs of
        ExprIdent q | isNothing q.qualifier ->
          BinderNamed { pos: q.pos, name: q.name } (toBinder rhs)
        _ -> BinderError "only a name can be given to a pattern with `@`"
    | otherwise -> BinderOp (toBinder lhs) op (toBinder rhs)
  ExprApp f a -> applyBinder (toBinder f) (toBinder a)
  _ -> BinderError "this is not a pattern"
  where
  applyBinder f a = case f of
    BinderConstructor n args -> BinderConstructor n (args <> [ a ])
    _ -> BinderError "only a constructor can be applied in a pattern"

mapLabeled :: forall a b. (a -> b) -> RecordLabeled a -> RecordLabeled b
mapLabeled f = case _ of
  RecordPun l -> RecordPun l
  RecordField l a -> RecordField l (f a)

-- | `x = e` in a `let` is both a name binding with no arguments and a pattern
-- | binding, and an LR automaton cannot tell them apart. Both are parsed as a
-- | binder and told apart here, the way upstream's `%shift` on `binderAtom ->
-- | ident` tells them apart there.
letBinding :: Binder -> Guarded -> LetBinding
letBinding b g = case b of
  BinderVar n -> LetName n [] g
  BinderConstructor q args
    | isNothing q.qualifier
    , isLowerName q.name -> LetName { pos: q.pos, name: q.name } args g
  _ -> LetPattern b g

-- | Whether a name is one a value binding could have. A constructor pattern
-- | whose head is lower case was a function definition all along.
isLowerName :: String -> Boolean
isLowerName text = case Array.head (SCU.toCharArray text) of
  Just c -> c >= 'a' && c <= 'z' || c == '_'
  Nothing -> false

-- | The prefix of a class head, read as a class head once the token after it
-- | has said which of the two it was.
classHead :: Array Constraint -> Qual -> Array Type -> Maybe (Array Fundep) -> ClassHead
classHead super n vars fundeps = ClassHead
  { super
  , name: n
  , vars: map typeToVarBinding vars
  , fundeps: case fundeps of
      Just fs -> fs
      Nothing -> []
  }

-- | The type atoms of a class head, read as the type variables they were.
-- |
-- | `(a :: Kind)` parses as a row with one label, because that is what it is
-- | everywhere else, and this is where it becomes a kinded type variable. This
-- | is the same ambiguity upstream describes in its `classHead` comment and
-- | resolves with backtracking.
typeToVarBinding :: Type -> TypeVarBinding
typeToVarBinding = case _ of
  TypeVar n -> TypeVarName n
  TypeRow (Row [ RowLabel l k ] Nothing) -> TypeVarKinded l k
  TypeParens t -> typeToVarBinding t
  _ -> TypeVarName { pos: { line: 0, column: 0 }, name: "?" }

-- | Where a declaration was that could not be read.
-- |
-- | The error itself is not kept here -- a recovering parse hands back every
-- | one of them alongside the tree, so keeping a copy in the tree would be
-- | keeping it twice. What the tree needs is that there was a hole and where
-- | it was, which is what the token the parse stopped on says.
brokenAt :: ParseError T.SourceToken -> Maybe T.SourcePos
brokenAt error = map (\t -> t.range.start) error.found

declClass :: ClassHead -> Array Labeled -> Decl
declClass = DeclClass

moduleOf :: Qual -> Maybe (Array Export) -> Array DeclChain -> Module
moduleOf n exports chains =
  Module (plain n) exports (Array.mapMaybe importOf chains)
    (Array.concat (map declsOf chains))
  where
  importOf = case _ of
    ChainImport i -> Just i
    _ -> Nothing

  -- The hole goes into the declarations, in the place the unreadable one was.
  -- Dropping it here would leave a `Module` that reads as though the file had
  -- one declaration fewer and nothing wrong with it, which is the opposite of
  -- what recovering is for: the errors come back beside the tree, and the tree
  -- is what says where they were.
  declsOf = case _ of
    ChainImport _ -> []
    ChainDecls ds -> ds
    ChainBroken at -> [ DeclBroken at ]
