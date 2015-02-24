{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -w #-}

module Tonic where

import           Control.Monad.State.Lazy
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

------------------------------------------------------------------------

pattern F32 = Fmt R 32

foo0 :: Term Int
foo0 =
    Let [x] (CallBinary (Add F32) (Num 42 F32) (Num 18 F32)) $
    Return (CallStatic float2string [Var x])
  where
    x = 1

foo1 :: Term Int
foo1 =
    Let [x] (Copy [Num 9 F32]) $
    Let [y] (Copy [Num 1 F32]) $
    Let [z] (CallBinary (Add F32) (Var x) (Var y)) $
    Return (CallStatic float2string [Var z])
  where
    (x,y,z) = (1,2,3)

foo2 :: Term String
foo2 =
    letrec [ (x, Lambda []  (Return (Copy [Num 9 F32])))
           , (y, Const      (Return (Copy [Num 1 F32])))
           , (f, Lambda [w] (Return (Copy [Var x])))
           , (g, Lambda []  (Return (Copy [Var u]))) ] $
    Let [z] (CallBinary (Add F32) (Var x) (Var y)) $
    Return (CallStatic float2string [Var t])
  where
    (x,y,z,w,u,t,f,g,h) = ("x","y","z","w","u","t","f","g","h")

    letrec bs t = LetRec (M.fromList bs) t

float2string :: SMethod
float2string = SMethod "java/lang/Float" "toString" [Float] (Just (Object "java/lang/String"))

------------------------------------------------------------------------

data Atom n =
      Var n
    | Num Rational Format
    | Str Text
    deriving (Eq, Ord, Show)

type Bindings n = Map n (Binding n)

data Binding n =
      Lambda [n] (Term n)
    | Const      (Term n)
    deriving (Eq, Ord, Show)

data Term n =
      Return (Tail n)
    | Let    [n] (Tail n) (Term n)
    | LetRec (Bindings n) (Term n)
    | Iff    (Atom n) (Term n) (Term n)
    deriving (Eq, Ord, Show)

data Tail n =
      Copy [Atom n]

    | Call        (Atom n) [Atom n]
    | CallUnary   UnaryOp  (Atom n)
    | CallBinary  BinaryOp (Atom n) (Atom n)
    | CallVirtual IMethod  (Atom n) [Atom n]
    | CallSpecial IMethod  (Atom n) [Atom n]
    | CallStatic  SMethod  [Atom n]

    | GetField  IField (Atom n)
    | SetField  IField (Atom n) (Atom n)
    | GetStatic SField
    | SetStatic SField (Atom n)
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
      N -- ^ natural numbers
    | Z -- ^ integers
    | R -- ^ rationals
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

------------------------------------------------------------------------
-- Finding Free Variables

fvOfAtom :: Ord n => Atom n -> Set n
fvOfAtom e = case e of
    Var x   -> S.singleton x
    Num _ _ -> S.empty
    Str _   -> S.empty

fvOfAtoms :: Ord n => [Atom n] -> Set n
fvOfAtoms = S.unions . map fvOfAtom

fvOfBinding :: Ord n => Binding n -> Set n
fvOfBinding e = case e of
    Lambda ns x -> fvOfTerm x `differenceL` ns
    Const     x -> fvOfTerm x

fvOfBindings :: Ord n => Bindings n -> Set n
fvOfBindings = S.unions . map fvOfBinding . M.elems

bvOfBindings :: Ord n => Bindings n -> Set n
bvOfBindings = M.keysSet

fvOfTerm :: Ord n => Term n -> Set n
fvOfTerm e = case e of
    Return x      -> fvOfTail x
    Let    ns x y -> fvOfTail x `S.union` (fvOfTerm y `differenceL` ns)
    LetRec bs x   -> (fvOfBindings bs `S.union` fvOfTerm x) `S.difference` bvOfBindings bs
    Iff    i t e  -> fvOfAtom i `S.union` fvOfTerm t `S.union` fvOfTerm e

fvOfTail :: Ord n => Tail n -> Set n
fvOfTail e = case e of
    Copy xs            -> fvOfAtoms xs
    Call f xs          -> fvOfAtom f `S.union` fvOfAtoms xs
    CallUnary   _ x    -> fvOfAtom x
    CallBinary  _ x y  -> fvOfAtom x `S.union` fvOfAtom y
    CallVirtual _ i xs -> fvOfAtom i `S.union` fvOfAtoms xs
    CallSpecial _ i xs -> fvOfAtom i `S.union` fvOfAtoms xs
    CallStatic  _   xs -> fvOfAtoms xs
    GetField    _ i    -> fvOfAtom i
    SetField    _ i x  -> fvOfAtom i `S.union` fvOfAtom x
    GetStatic   _      -> S.empty
    SetStatic   _   x  -> fvOfAtom x

------------------------------------------------------------------------
-- Capture Avoiding Substitution


------------------------------------------------------------------------
-- Utils

differenceL :: Ord a => Set a -> [a] -> Set a
differenceL s xs = S.difference s (S.fromList xs)
