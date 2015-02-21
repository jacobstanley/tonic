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

pattern F32 = Fmt R 32

foo0 :: Expr Name
foo0 =
    Let (Locl 1 F32) (Var (NLit 9 F32)) $
    Let (Locl 2 F32) (Var (NLit 1 F32)) $
    Let (Locl 3 F32) (Var (Prim Add F32) `App` Var (Locl 1 F32) `App` Var (Locl 2 F32)) $
    Var float2string `App` Var (Locl 3 F32)

foo1 :: Expr Name
foo1 =
    Var (Prim Add F32) `App` Var (NLit 42 F32) `App` Var (NLit 18 F32)

float2string :: Name
float2string = SMth $ MethodRef (ClassRef "java/lang/Float") (NameType "toString" (Type "(F)Ljava/lang/String;"))

type Instructions = [Instruction]

foo :: Expr Name
foo = snd (unE expr 1)
  where
    expr = add `app` n1 `app` n2
    add = lam $ \x -> lam $ \y -> var float2string `app` (var (Prim Add F32) `app` x `app` y)
    n1 = var (NLit 1 F32)
    n2 = var (NLit 9 F32)

------------------------------------------------------------------------

class Lambda exp where
    lam :: (exp -> exp) -> exp
    app :: exp -> (exp -> exp)

newtype E = E { unE :: Integer -> (Integer, Expr Name) }

instance Lambda E where
    lam f = E $ \i -> let n      = Anon i               in
                      let v      = E (\j -> (j, Var n)) in
                      let (j, x) = unE (f v) (i+1)      in
                      (j, Lam n x)

    app f x = E $ \i -> let (j, f') = unE f i in
                        let (k, x') = unE x j in
                        (k, App f' x')

var :: Name -> E
var n = E (\i -> (i, Var n))

------------------------------------------------------------------------

data Name =
      Anon Integer         -- ^ an anonymous variable
    | Locl VarIndex Format -- ^ local variable
    | NLit Rational Format -- ^ numeric literal
    | SLit Text            -- ^ string literal
    | Prim PrimOp   Format -- ^ primitive operation
    | IFld FieldRef        -- ^ instance field
    | SFld FieldRef        -- ^ static field
    | VMth MethodRef       -- ^ virtual method
    | SMth MethodRef       -- ^ static method
    deriving (Eq, Ord, Show)

data Expr n =
      Var n
    | Lam n (Expr n)
    | App   (Expr n) (Expr n)
    | Let n (Expr n) (Expr n)
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

data PrimOp =
      Neg        -- ^ negation
    | Not        -- ^ logical NOT
    | Cnv Format -- ^ convert to format

    | Add -- ^ addition
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

compileFunc :: Expr Name -> Instructions
compileFunc term = compileExpr term <> pure AReturn

compileExpr :: Expr Name -> Instructions
compileExpr expr = case expr of
    Var n       -> load n
    App f x     -> compileExpr x <> compileExpr f
    Let n x1 x2 -> compileExpr x1 <> store n <> compileExpr x2

    x -> error ("compileExpr: cannot generate instuctions for: " <> show x)

load :: Name -> Instructions
load n = case n of
    Locl i (Fmt Z s) | s <= 32 -> pure (ILoad i)
                     | s <= 64 -> pure (LLoad i)
    Locl i (Fmt R s) | s <= 32 -> pure (FLoad i)
                     | s <= 64 -> pure (DLoad i)

    NLit x (Fmt Z s) | s <= 32 -> pure (IConst (truncate x))
                     | s <= 64 -> pure (LConst (truncate x))
    NLit x (Fmt R s) | s <= 32 -> pure (FConst (fromRational x))
                     | s <= 64 -> pure (DConst (fromRational x))
    SLit x                     -> pure (SConst x)

    Prim Add (Fmt Z s) | s <= 32 -> pure IAdd
                       | s <= 64 -> pure LAdd
    Prim Add (Fmt R s) | s <= 32 -> pure FAdd
                       | s <= 64 -> pure DAdd

    SMth ref -> pure (InvokeStatic ref)
    VMth ref -> pure (InvokeVirtual ref)

    _ -> error ("load: cannot load: " <> show n)

store :: Name -> Instructions
store n = case n of
    Locl i (Fmt Z s) | s <= 32 -> pure (IStore i)
                     | s <= 64 -> pure (LStore i)
    Locl i (Fmt R s) | s <= 32 -> pure (FStore i)
                     | s <= 64 -> pure (DStore i)

    _ -> error ("store: cannot store to: " <> show n)

------------------------------------------------------------------------

fvOfExpr :: Ord n => Expr n -> Set n
fvOfExpr expr = case expr of
    Var n       -> S.singleton n
    Lam n x     -> S.delete n (fvOfExpr x)
    App f x     -> fvOfExpr f `S.union` fvOfExpr x
    Let n x1 x2 -> fvOfExpr x1 `S.union` S.delete n (fvOfExpr x2)
