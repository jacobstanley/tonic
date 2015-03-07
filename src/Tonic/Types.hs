module Tonic.Types where

import Data.Map (Map)
import Data.Text (Text)

------------------------------------------------------------------------

data Atom n =
      Var n
    | Num Rational Format
    | Str Text
    deriving (Eq, Ord, Show)

data Tail n =
      Copy [Atom n]

    | Invoke        FunType  (Atom n) [Atom n]
    | InvokeUnary   UnaryOp  (Atom n)
    | InvokeBinary  BinaryOp (Atom n) (Atom n)
    | InvokeVirtual VMethod  (Atom n) [Atom n]
    | InvokeSpecial VMethod  (Atom n) [Atom n]
    | InvokeStatic  SMethod  [Atom n]

    | GetField  IField (Atom n)
    | PutField  IField (Atom n) (Atom n)
    | GetStatic SField
    | PutStatic SField (Atom n)

    | New Constructor [Atom n]

    deriving (Eq, Ord, Show)

type Bindings n = Map n (Binding n)

data Binding n =
      Lambda FunType [n] (Term n)
    | Const  Type        (Term n)
    deriving (Eq, Ord, Show)

data Term n =
      Return (Tail n)
    | Iff    (Atom n) (Term n) (Term n)
    | Let    [n] (Tail n) (Term n)
    | LetRec (Bindings n) (Term n)
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

data UnaryOp =
      Neg Format        -- ^ negation
    | Not Format        -- ^ logical NOT
    | Cnv Format Format -- ^ convert from/to format
    deriving (Eq, Ord, Show)

data BinaryOp =
      Add Format -- ^ addition
    | Sub Format -- ^ subtraction
    | Mul Format -- ^ multiplication
    | Div Format -- ^ division
    | Rem Format -- ^ remainder

    | Sla Format -- ^ shift left  (arithmetic)
    | Sra Format -- ^ shift right (arithmetic)
    | Sru Format -- ^ shift right (unsigned / logical)

    | And Format -- ^ bitwise AND
    | Ior Format -- ^ bitwise Inclusive OR
    | Xor Format -- ^ bitwise eXclusive OR

    | Ceq Format -- ^ equal
    | Cne Format -- ^ not equal
    | Clt Format -- ^ less than
    | Cgt Format -- ^ greater than
    | Cle Format -- ^ less or equal
    | Cge Format -- ^ greater or equal
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

-- | Describes the representation of a number.
data Format = Fmt Genre Size
    deriving (Eq, Ord, Show)

-- | The encoding used to represent a number.
data Genre =
      U -- ^ unsigned integers
    | I -- ^ signed integers
    | F -- ^ floating point
    | A -- ^ objects (can only be null)
    deriving (Eq, Ord, Enum, Show)

-- | Number of bits to use to represent a number.
type Size = Integer

-- | Value types - the type of anything assignable to a variable.
data Type =
      FunTy FunType
    | NumTy Format
    | ObjTy Text
    | ArrTy Type
    deriving (Eq, Ord, Show)

-- | Function types.
data FunType = FunType [Type] [Type]
    deriving (Eq, Ord, Show)

-- | Method types - methods are functions that cannot be assigned to
-- variables and can only have one or zero returns values.
data MethodType = MethodType [Type] (Maybe Type)
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

type ClassName  = Text
type MethodName = Text
type FieldName  = Text

-- | Constructors.
data Constructor = Constructor ClassName MethodType
    deriving (Eq, Ord, Show)

-- | Virtual method.
data VMethod = VMethod ClassName MethodName MethodType
    deriving (Eq, Ord, Show)

-- | Static method.
data SMethod = SMethod ClassName MethodName MethodType
    deriving (Eq, Ord, Show)

-- | Interface method.
data IMethod = IMethod ClassName MethodName MethodType
    deriving (Eq, Ord, Show)

-- | Instance field.
data IField = IField ClassName FieldName Type
    deriving (Eq, Ord, Show)

-- | Static field.
data SField = SField ClassName FieldName Type
    deriving (Eq, Ord, Show)
