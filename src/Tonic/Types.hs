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

    | Invoke        (Atom n) [Atom n]
    | InvokeUnary   UnaryOp  (Atom n)
    | InvokeBinary  BinaryOp (Atom n) (Atom n)
    | InvokeVirtual IMethod  (Atom n) [Atom n]
    | InvokeSpecial IMethod  (Atom n) [Atom n]
    | InvokeStatic  SMethod  [Atom n]

    | GetField  IField (Atom n)
    | PutField  IField (Atom n) (Atom n)
    | GetStatic SField
    | PutStatic SField (Atom n)
    deriving (Eq, Ord, Show)

type Bindings n = Map n (Binding n)

data Binding n =
      Lambda [n] (Term n)
    | Const      (Term n)
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

data Format = Fmt Genre Size
    deriving (Eq, Ord, Show)

data Genre =
      U -- ^ unsigned integers
    | I -- ^ signed integers
    | F -- ^ floating point
    deriving (Eq, Ord, Enum, Show)

type Size = Integer

------------------------------------------------------------------------

-- | JVM types.
data JType =
      Boolean
    | Byte
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double
    | Object Text
    deriving (Eq, Ord, Show)

type ClassName  = Text
type MethodName = Text
type FieldName  = Text

-- | Instance method.
data IMethod = IMethod ClassName MethodName [JType] (Maybe JType)
    deriving (Eq, Ord, Show)

-- | Static method.
data SMethod = SMethod ClassName MethodName [JType] (Maybe JType)
    deriving (Eq, Ord, Show)

-- | Instance field.
data IField = IField ClassName FieldName JType
    deriving (Eq, Ord, Show)

-- | Static field.
data SField = SField ClassName FieldName JType
    deriving (Eq, Ord, Show)
