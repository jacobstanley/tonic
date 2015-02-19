{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -w #-}

module Tonic where

import           Control.Applicative
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S

import           JVM.Codegen

------------------------------------------------------------------------

foo0 :: Term Var
foo0 =
    Let (V I32 1) (Ret (Imm 42 I32)) $
    Let (V I32 2) (Ret (Imm 18 I32)) $
    Ret (Op2 Add I32 (Var (V I32 1))
                     (Var (V I32 2)))

foo :: Term Var
foo =
    Ret (Op2 Add I32 (Imm 42 I32)
                     (Imm 18 I32))

------------------------------------------------------------------------

data Var = V Format VarIndex
    deriving (Eq, Ord, Show)

pattern I32 = Fmt Z 32

type Instructions = [Instruction]

------------------------------------------------------------------------

compileFunc :: Term Var -> Instructions
compileFunc term = compileTerm term <> pure IReturn

compileTerm :: Term Var -> Instructions
compileTerm term = case term of
    Ret x       -> compileAtom x
    Let v t1 t2 -> compileTerm t1 <> store v <> compileTerm t2

    x -> error ("compileTerm: cannot generate instuctions for: " <> show x)

compileAtom :: Atom Var -> Instructions
compileAtom atom = case atom of
    Var v           -> load v
    Imm x I32       -> pure (IConst (round x))
    Op2 Add I32 x y -> compileAtom x <> compileAtom y <> pure IAdd

    x -> error ("compileAtom: cannot generate instuctions for: " <> show x)

load :: Var -> Instructions
load (V I32 x) = pure (ILoad x)
load v         = error ("load: cannot load: " <> show v)

store :: Var -> Instructions
store (V I32 x) = pure (IStore x)
store v         = error ("store: cannot store: " <> show v)

------------------------------------------------------------------------

fvOfAtom :: Ord a => Atom a -> Set a
fvOfAtom atom = case atom of
    Var v         -> S.singleton v
    Imm _ _       -> S.empty
    Op1 _ _ x     -> fvOfAtom x
    Op2 _ _ x1 x2 -> fvOfAtom x1 `S.union` fvOfAtom x2

fvOfTerm :: Ord a => Term a -> Set a
fvOfTerm term = case term of
    Ret x       -> fvOfAtom x
    Let v t1 t2 -> fvOfTerm t1 `S.union` S.delete v (fvOfTerm t2)

------------------------------------------------------------------------

data Atom v =
      Var v                                 -- ^ variables
    | Imm Rational Format                   -- ^ constants
    | Op1 UnaryOp  Format (Atom v)          -- ^ unary operations
    | Op2 BinaryOp Format (Atom v) (Atom v) -- ^ binary operations
    deriving (Eq, Ord, Show)

data Term v =
      Ret (Atom v)            -- ^ lifted atom
    | Let v (Term v) (Term v) -- ^ let binding
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

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

data Format = Fmt Genre Size
    deriving (Eq, Ord, Show)

data Genre =
      N -- ^ natural numbers
    | Z -- ^ signed integers
    | R -- ^ rationals
    deriving (Eq, Ord, Enum, Show)

type Size = Integer
