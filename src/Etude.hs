module Etude where

import Data.Map (Map)
import Data.Set (Set)

------------------------------------------------------------------------

type Atoms a = [Atom a]

data Atom a =
      Var a                                 -- ^ variable atoms
    | Imm Rational Format                   -- ^ constant atoms
    | Op1 UnaryOp  Format (Atom a)          -- ^ unary operations
    | Op2 BinaryOp Format (Atom a) (Atom a) -- ^ binary operations
    deriving (Eq, Ord)

data UnaryOp =
      Neg        -- ^ negation
    | Not        -- ^ logical NOT
    | Cnv Format -- ^ convert to format
    deriving (Eq, Ord, Show)

data BinaryOp =
      Add -- ^ addition
    | Sub -- ^ subtraction
    | Mul -- ^ multiplication
    | Div -- ^ division
    | Rem -- ^ remainder

    | Shl -- ^ shift left
    | Shr -- ^ shift right (arithmetic)
    | Uhr -- ^ shift right (logical / unsigned)

    | And -- ^ bitwise AND
    | Ior -- ^ bitwise Inclusive OR
    | Xor -- ^ bitwise eXclusive OR

    | Ceq -- ^ equal
    | Cne -- ^ not equal
    | Clt -- ^ less than
    | Cgt -- ^ greater than
    | Cle -- ^ less or equal
    | Cge -- ^ greater or equal
    deriving (Eq, Ord, Enum, Show)

------------------------------------------------------------------------

data Term a =
      Ret          (Atoms a) -- ^ lifted atoms
    | App (Atom a) (Atoms a) -- ^ function application
    | Sys Prim     (Atoms a) -- ^ primitive operation

    | Iff (Atom a) (Term a) (Term a) -- ^ conditional expressions

    | New Envelope                                   -- ^ object introduction
    | Del Envelope                                   -- ^ object destruction
    | Get (Atom a, Set Attr) Format                  -- ^ object inspection
    | Set (Atom a, Set Attr) Format {-to-} (Atom a)  -- ^ object adjustment
    | Ini (Atom a, Set Attr) Format {-to-} (Atom a)  -- ^ object initialisation

    | Let (Bindings a) (Term a) -- ^ monadic bindings
    deriving (Eq, Ord)

------------------------------------------------------------------------

data Format = Fmt Genre Encoding
    deriving (Eq, Ord, Show)

data Genre =
      NG -- ^ natural numbers
    | ZG -- ^ signed integers
    | RG -- ^ rationals
    | FG -- ^ functions
    | OG -- ^ objects
    deriving (Eq, Ord, Enum, Show)

type Encoding = Integer

type Prim = Integer

data Attr =
      CM -- ^ constant
    | VM -- ^ volatile
    deriving (Eq, Ord, Enum, Show)

------------------------------------------------------------------------

type Parameters a = [a]

-- | Individual term binding.
type Binding a = (Parameters a, Term a)

type Bindings a = [Binding a]

type FEnv a = Map Integer (Function a)

data Function a = Fun (Parameters a) (Term a)
    deriving (Eq, Ord)

type OEnv = (M, A, Envelope, Envelope, Stack, Integer, Integer, Integer)
type M = Map Integer Integer
type A = Map Integer Bool

type Stack      = [StackFrame]
type StackFrame = (Integer, Envelope)

type Envelope        = Set EnvelopeElement
type EnvelopeElement = (Integer, Format, Set Attr)

type Modules a = Map (Module a) ((Map (a) (Atom a)))

data Module a = Module (Term a) {-export-} (Exports a) {-where-} (Items a)
    deriving (Eq, Ord)

type Exports a = Map (String) (a)

type Items a = Map a (Item a)

data Item a =
      FunI (Function a)
    | ObjI (Data a) {-of-} Envelope
    | ImpI String
    deriving (Eq, Ord)

type Data a = Set (Integer, Format, Set Attr, Atom a)
