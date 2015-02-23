{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -w #-}

module Tonic where

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

import           JVM.Codegen

------------------------------------------------------------------------

pattern F32 = Fmt R 32

foo0 :: Term Int
foo0 =
    Let [x] (Copy [Num 9 F32]) $
    Let [y] (Copy [Num 1 F32]) $
    Let [z] (CallBinary (Add F32) (Var x) (Var y)) $
    Return (CallStatic float2string [Var z])
  where
    (x,y,z) = (1,2,3)

foo1 :: Term Int
foo1 =
    Let [x] (CallBinary (Add F32) (Num 42 F32) (Num 18 F32)) $
    Return (CallStatic float2string [Var x])
  where
    x = 1

float2string :: MethodRef
float2string = MethodRef (ClassRef "java/lang/Float") (NameType "toString" (Type "(F)Ljava/lang/String;"))

type Instructions = [Instruction]

------------------------------------------------------------------------

data Atom n =
      Var n
    | Num Rational Format
    | Str Text
    deriving (Eq, Ord, Show)

data Term n =
      Return (Tail n)
    | Let    [n] (Tail n) (Term n)
    | LetRec (Map n (Binding n)) (Term n)
    | Iff    (Atom n) (Term n) (Term n)
    deriving (Eq, Ord, Show)

data Binding n =
      Lambda [n] (Term n)
    | Const      (Term n)
    deriving (Eq, Ord, Show)

data Tail n =
      Copy [Atom n]

    | Call        (Atom n)  [Atom n]
    | CallUnary   UnaryOp   (Atom n)
    | CallBinary  BinaryOp  (Atom n) (Atom n)
    | CallStatic  MethodRef [Atom n]
    | CallVirtual MethodRef (Atom n) [Atom n]

    | GetField  FieldRef (Atom n)
    | SetField  FieldRef (Atom n) (Atom n)
    | GetStatic FieldRef
    | SetStatic FieldRef (Atom n)
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

{-
compileFunc :: Term Int -> Instructions
compileFunc term = compileTerm term <> pure AReturn

compileTerm :: Term Int -> Instructions
compileTerm term = case term of
    Var n       -> load n
    App f x     -> compileTerm x <> compileTerm f
    Let n x1 x2 -> compileTerm x1 <> store n <> compileTerm x2

    x -> error ("compileTerm: cannot generate instuctions for: " <> show x)

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

newtype Beta a = Beta { unBeta :: State Bool a }
  deriving (Functor, Applicative, Monad, MonadState Bool)

runBeta :: Beta a -> (a, Bool)
runBeta m = runState (unBeta m) False

betaReduce :: Eq n => Term n -> Term n
betaReduce e = case runBeta (step e) of
      (e', True)  -> betaReduce e'
      (e', False) -> e'
 where
    step term = case term of
      Var n       -> pure (Var n)
      Lam n x     -> Lam <$> pure n <*> step x
      Let n x1 x2 -> Let <$> pure n <*> step x1 <*> step x2

      App (Lam n t) s -> pure (subst n s t) <* put True
      App f x         -> App <$> step f <*> step x

subst :: Eq n => n -> Term n -> Term n -> Term n
subst old new term = case term of
    Var n | n == old  -> new
          | otherwise -> Var n

    Lam n x | n == old  -> Lam n x
            | otherwise -> Lam n (subst' x)

    App f x -> App (subst' f) (subst' x)

    Let n x1 x2 | n == old  -> Let n (subst' x1) x2
                | otherwise -> Let n (subst' x1) (subst' x2)
  where
    subst' = subst old new

fvOfExpr :: Ord n => Term n -> Set n
fvOfExpr term = case term of
    Var n       -> S.singleton n
    Lam n x     -> S.delete n (fvOfExpr x)
    App f x     -> fvOfExpr f `S.union` fvOfExpr x
    Let n x1 x2 -> fvOfExpr x1 `S.union` S.delete n (fvOfExpr x2)
-}
