{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Tonic where

import           Control.Applicative
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           JVM.Codegen

------------------------------------------------------------------------

foo :: Term Name
foo =
    Let (LV 1 F32) (Ret (Imm 9 F32)) $
    Let (LV 2 F32) (Ret (Imm 1 F32)) $
    Let (LV 3 F32) (App (Op2 Add F32) [Var (LV 1 F32), Var (LV 2 F32)]) $
    App float2string [Var (LV 3 F32)]

foo1 :: Term Name
foo1 =
    App (Op2 Add F32) [Imm 42 F32, Imm 18 F32]

float2string :: Atom Name
float2string = Var $ SM $ MethodRef (ClassRef "java/lang/Float") (NameType "toString" (Type "(F)Ljava/lang/String;"))

------------------------------------------------------------------------

data Name =
      LV VarIndex Format
    | SF FieldRef
    | IF FieldRef
    | SM MethodRef
    | VM MethodRef
    deriving (Eq, Ord, Show)

pattern F32 = Fmt R 32

type Instructions = [Instruction]

------------------------------------------------------------------------

compileFunc :: Term Name -> Instructions
compileFunc term = compileTerm term <> pure AReturn

compileTerm :: Term Name -> Instructions
compileTerm term = case term of
    Ret x                 -> compileAtom x
    App f xs              -> concatMap compileAtom xs <> compileAtom f
    Let (LV ix fmt) t1 t2 -> compileTerm t1 <> store ix fmt <> compileTerm t2

    x -> error ("compileTerm: cannot generate instuctions for: " <> show x)

compileAtom :: Atom Name -> Instructions
compileAtom expr = case expr of
    Var (LV ix fmt) -> load ix fmt
    Var (SM ref)    -> pure (InvokeStatic ref)
    Var (VM ref)    -> pure (InvokeVirtual ref)

    Imm x (Fmt Z s) | s <= 32 -> pure (IConst (truncate x))
                    | s <= 64 -> pure (LConst (truncate x))
    Imm x (Fmt R s) | s <= 32 -> pure (FConst (fromRational x))
                    | s <= 64 -> pure (DConst (fromRational x))

    Op2 Add (Fmt Z s) | s <= 32 -> pure IAdd
                      | s <= 64 -> pure LAdd
    Op2 Add (Fmt R s) | s <= 32 -> pure FAdd
                      | s <= 64 -> pure DAdd

    x -> error ("compileAtom: cannot generate instuctions for: " <> show x)

load :: VarIndex -> Format -> Instructions
load ix fmt = case fmt of
    (Fmt Z s) | s <= 32 -> pure (ILoad ix)
              | s <= 64 -> pure (LLoad ix)
    (Fmt R s) | s <= 32 -> pure (FLoad ix)
              | s <= 64 -> pure (DLoad ix)
    _ -> error ("load: cannot load: " <> show fmt)

store :: VarIndex -> Format -> Instructions
store ix fmt = case fmt of
    (Fmt Z s) | s <= 32 -> pure (IStore ix)
              | s <= 64 -> pure (LStore ix)
    (Fmt R s) | s <= 32 -> pure (FStore ix)
              | s <= 64 -> pure (DStore ix)
    _ -> error ("store: cannot store: " <> show fmt)

------------------------------------------------------------------------

fvOfAtom :: Ord n => Atom n -> Set n
fvOfAtom expr = case expr of
    Var n   -> S.singleton n
    Imm _ _ -> S.empty
    Op1 _ _ -> S.empty
    Op2 _ _ -> S.empty

fvOfTerm :: Ord n => Term n -> Set n
fvOfTerm term = case term of
    Ret x       -> fvOfAtom x
    App f xs    -> fvOfAtom f `S.union` S.unions (map fvOfAtom xs)
    Let n t1 t2 -> fvOfTerm t1 `S.union` S.delete n (fvOfTerm t2)

------------------------------------------------------------------------

type Atoms n = [Atom n]

data Atom n =
      Var n               -- ^ variables
    | Imm Rational Format -- ^ constants
    | Op1 UnaryOp  Format -- ^ unary operations
    | Op2 BinaryOp Format -- ^ binary operations
    deriving (Eq, Ord, Show)

data Term n =
      Ret (Atom n)            -- ^ lifted atom
    | App (Atom n) (Atoms n)  -- ^ function application
    | Let n (Term n) (Term n) -- ^ monadic bindings
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
    | Z -- ^ integers
    | R -- ^ rationals
    deriving (Eq, Ord, Enum, Show)

type Size = Integer
