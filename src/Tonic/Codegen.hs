{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Tonic.Codegen where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import qualified JVM.Codegen as G
import           Tonic.Types
import           Tonic.Utils

------------------------------------------------------------------------

data Local = L G.VarIndex Format
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

closureOfBinding :: (Ord n, Show n) => ClassName -> Binding n -> G.Class
closureOfBinding name (Lambda ftyp@(FunType ins outs) ns x) =
      addInterface (mangleFunType ftyp)
    . addMethod "invoke" mtyp (G.Code 8 8 [ret])
    $ mkClass ("C$" <> name)
  where
    (ret, mtyp) = case outs of
        []  -> (G.Return,                 MethodType ins Nothing)
        [o] -> (iReturn (formatOfType o), MethodType ins (Just o))
        _   -> error ("closureOfBinding: multiple return values not supported: " <> show name)

codeOfTail :: (Ord n, Show n) => Map n Local -> Tail n -> [G.Instruction]
codeOfTail env tl = case tl of
    Copy xs             -> map (iPush env) xs
    InvokeUnary  op x   -> [iPush env x, iUnary op]
    InvokeBinary op x y -> [iPush env x, iPush env y, iBinary op]
    _                   -> error ("codeOfTail: unsupported: " <> show tl)

-- InvokeVirtual IMethod  (Atom n) [Atom n]
-- InvokeSpecial IMethod  (Atom n) [Atom n]
-- InvokeStatic  SMethod  [Atom n]

codeOfTerm :: (Ord n, Show n) => [G.VarIndex] -> Map n Local -> Term n -> ([G.Instruction], [G.Class])
codeOfTerm env term = case term of
    Return x   -> (codeOfTail env x, [])
    Let ns x y -> codeOfTail env x <> map

------------------------------------------------------------------------

iUnary :: UnaryOp -> G.Instruction
iUnary op = case op of
    Neg (Fmt I s) | s <= 32 -> G.INeg
                  | s <= 64 -> G.LNeg
    Neg (Fmt F s) | s <= 32 -> G.FNeg
                  | s <= 64 -> G.DNeg

    _ -> error ("iUnary: cannot invoke unary op: " <> show op)

iBinary :: BinaryOp -> G.Instruction
iBinary op = case op of
    Add (Fmt I s) | s <= 32 -> G.IAdd
                  | s <= 64 -> G.LAdd
    Add (Fmt F s) | s <= 32 -> G.FAdd
                  | s <= 64 -> G.DAdd

    Sub (Fmt I s) | s <= 32 -> G.ISub
                  | s <= 64 -> G.LSub
    Sub (Fmt F s) | s <= 32 -> G.FSub
                  | s <= 64 -> G.DSub

    Mul (Fmt I s) | s <= 32 -> G.IMul
                  | s <= 64 -> G.LMul
    Mul (Fmt F s) | s <= 32 -> G.FMul
                  | s <= 64 -> G.DMul

    Div (Fmt I s) | s <= 32 -> G.IDiv
                  | s <= 64 -> G.LDiv
    Div (Fmt F s) | s <= 32 -> G.FDiv
                  | s <= 64 -> G.DDiv

    Rem (Fmt I s) | s <= 32 -> G.IRem
                  | s <= 64 -> G.LRem
    Rem (Fmt F s) | s <= 32 -> G.FRem
                  | s <= 64 -> G.DRem

    _ -> error ("iBinary: cannot invoke binary op: " <> show op)

------------------------------------------------------------------------

iPush :: (Ord n, Show n) => Map n Local -> Atom n -> G.Instruction
iPush env atom = case atom of
    Var x     -> iLoad (unsafeLookup "iPush" x env)
    Num x fmt -> iConst x fmt
    Str x     -> G.SConst x

iConst :: Rational -> Format -> G.Instruction
iConst 0 (Fmt A _) = G.AConstNull
iConst x fmt       = case fmt of
    Fmt I s | s <= 32 -> G.IConst (truncate x)
            | s <= 64 -> G.LConst (truncate x)
    Fmt F s | s <= 32 -> G.FConst (fromRational x)
            | s <= 64 -> G.DConst (fromRational x)
    _                 -> error ("iConst: cannot load constant: " <> show x <> " (with format: " <> show fmt <> ")")

iLoad :: Local -> G.Instruction
iLoad (L idx fmt) = case fmt of
    Fmt A _           -> G.ALoad idx
    Fmt I s | s <= 32 -> G.ILoad idx
            | s <= 64 -> G.LLoad idx
    Fmt F s | s <= 32 -> G.FLoad idx
            | s <= 64 -> G.DLoad idx
    _                 -> error ("iLoad: cannot load format: " <> show fmt)

iStore :: Local -> G.Instruction
iStore (L idx fmt) = case fmt of
    Fmt A _           -> G.AStore idx
    Fmt I s | s <= 32 -> G.IStore idx
            | s <= 64 -> G.LStore idx
    Fmt F s | s <= 32 -> G.FStore idx
            | s <= 64 -> G.DStore idx
    _                 -> error ("iStore: cannot store format: " <> show fmt)

iReturn :: Format -> G.Instruction
iReturn (Fmt g s) = case g of
    U | s <= 32 -> G.IReturn
      | s <= 64 -> G.LReturn
    I | s <= 32 -> G.IReturn
      | s <= 64 -> G.LReturn
    F | s <= 32 -> G.FReturn
      | s <= 64 -> G.DReturn
    A           -> G.AReturn

------------------------------------------------------------------------

mkClass :: ClassName -> G.Class
mkClass name = G.Class
    { G.cAccess     = [ G.C'Public, G.C'Final, G.C'Super ]
    , G.cName       = G.ClassRef name
    , G.cSuper      = G.ClassRef "java/lang/Object"
    , G.cInterfaces = []
    , G.cFields     = []
    , G.cMethods    = []
    , G.cSourceFile = Nothing
    }

addInterface :: ClassName -> G.Class -> G.Class
addInterface name cls = cls { G.cInterfaces = G.cInterfaces cls ++ [G.ClassRef name] }

addField :: FieldName -> Type -> G.Class -> G.Class
addField = addFieldWith [G.F'Private, G.F'Final]

addStaticField :: FieldName -> Type -> G.Class -> G.Class
addStaticField = addFieldWith [G.F'Private, G.F'Static, G.F'Final]

addMethod :: MethodName -> MethodType -> G.Code -> G.Class -> G.Class
addMethod = addMethodWith [G.M'Public, G.M'Final]

addStaticMethod :: MethodName -> MethodType -> G.Code -> G.Class -> G.Class
addStaticMethod = addMethodWith [G.M'Public, G.M'Static, G.M'Final]

addFieldWith :: [G.FieldAccess] -> FieldName -> Type -> G.Class -> G.Class
addFieldWith acc name typ cls = cls { G.cFields = G.cFields cls ++ [fld] }
  where
    fld = G.Field acc name (G.Type (describeType typ))

addMethodWith :: [G.MethodAccess] -> MethodName -> MethodType -> G.Code -> G.Class -> G.Class
addMethodWith acc name typ code cls = cls { G.cMethods = G.cMethods cls ++ [mth] }
  where
    mth = G.Method acc name (G.Type (describeMethodType typ)) (Just code)

------------------------------------------------------------------------

formatOfType :: Type -> Format
formatOfType (NumTy f) = f
formatOfType _         = Fmt A 0

describeType :: Type -> Text
describeType vtyp = case vtyp of
    FunTy ftyp       -> "L" <> mangleFunType ftyp <> ";"
    NumTy (Fmt U 1)  -> "Z"
    NumTy (Fmt I 8)  -> "B"
    NumTy (Fmt U 16) -> "C"
    NumTy (Fmt I 16) -> "S"
    NumTy (Fmt I 32) -> "I"
    NumTy (Fmt I 64) -> "J"
    NumTy (Fmt F 32) -> "F"
    NumTy (Fmt F 64) -> "D"
    ObjTy name       -> "L" <> name <> ";"
    ArrTy etyp       -> "[" <> describeType etyp

describeMethodType :: MethodType -> Text
describeMethodType (MethodType args ret) = "(" <> args' <> ")" <> ret'
  where
    args' = T.concat (map describeType args)
    ret'  = maybe "V" describeType ret

mangleFunType :: FunType -> Text
mangleFunType (FunType ins outs) = "F" <> mangle ins <> "$$" <> mangle outs
  where
    mangle = T.replace "/" "_"
           . T.replace ";" "_$"
           . T.replace "[" "A"
           . T.concat
           . map describeType
