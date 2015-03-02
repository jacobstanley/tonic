{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Tonic.Codegen where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
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

closureOfBinding :: (Ord n, Show n) => ClassName -> Binding n -> (G.Class, [G.Class])
closureOfBinding name (Lambda ftyp@(FunType ins outs) ns x) = (cls, clss)
  where
    vars = [0..]
    ls   = zipWith L vars (map formatOfType ins)
    env  = M.fromList (ns `zip` ls)

    (code, clss) = codeOfTerm (drop (length ns) vars) env x

    cls = addInterface (mangleFunType ftyp)
        . addMethod "invoke" mtyp (G.Code 8 code)
        $ mkClass ("C$" <> name)

    mtyp = case outs of
        []  -> MethodType ins Nothing
        [o] -> MethodType ins (Just o)
        _   -> error $ "closureOfBinding: multiple return "
                    <> "values not supported: " <> show name

------------------------------------------------------------------------

codeOfTail :: (Ord n, Show n) => Map n Local -> Tail n -> [G.Instruction]
codeOfTail env tl = case tl of
    Copy xs              -> map (iPush env) xs
    InvokeUnary  op x    -> [iPush env x, iUnary op]
    InvokeBinary op x y  -> [iPush env x, iPush env y, iBinary op]
    InvokeVirtual m i xs -> map (iPush env) (i:xs) <> [G.InvokeVirtual (iMethodRef m)]
    InvokeSpecial m i xs -> map (iPush env) (i:xs) <> [G.InvokeSpecial (iMethodRef m)]
    InvokeStatic  m   xs -> map (iPush env) xs     <> [G.InvokeStatic  (sMethodRef m)]
    GetField      f i    -> [iPush env i]              <> [G.GetField  (iFieldRef f)]
    PutField      f i x  -> [iPush env i, iPush env x] <> [G.PutField  (iFieldRef f)]
    GetStatic     f      ->                               [G.GetStatic (sFieldRef f)]
    PutStatic     f   x  -> [iPush env x]              <> [G.PutStatic (sFieldRef f)]

--codeOfBindings :: (Ord n, Show n) => Bindings n -> ([G.Instruction], [G.Class])
--codeOfBindings bs = M.map

codeOfTerm :: (Ord n, Show n) => [G.VarIndex] -> Map n Local -> Term n -> ([G.Instruction], [G.Class])
codeOfTerm vars env term = case term of
    Return x   -> let code0 = codeOfTail env x
                      code1 = case formatsOfTail env x of
                        []    -> [G.Return]
                        [fmt] -> [iReturn fmt]
                        _     -> error $ "codeOfTerm: multiple return "
                                      <> "values not supported: " <> show x
                  in
                      (code0 <> code1, [])

    Let ns x y -> let code0       = codeOfTail env  x

                      (vars', ls) = allocLocals vars (formatsOfTail env x)
                      env'        = env `mapUnionR` M.fromList (ns `zip` ls)
                      code1       = map iStore ls

                      (code2, cs) = codeOfTerm vars' env' y
                  in
                      (code0 <> code1 <> code2, cs)

    _ -> error ("codeOfTerm: unsupported: " <> show term)


allocLocals :: [G.VarIndex] -> [Format] -> ([G.VarIndex], [Local])
allocLocals vs fs = (drop (length fs) vs, zipWith L vs fs)

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

formatOfLocal :: Local -> Format
formatOfLocal (L _ f) = f

formatsOfFunType :: FunType -> [Format]
formatsOfFunType (FunType _ os) = map formatOfType os

formatsOfMethodType :: MethodType -> [Format]
formatsOfMethodType (MethodType _ os) = map formatOfType (maybeToList os)

formatOfUnary :: UnaryOp -> Format
formatOfUnary op = case op of
    Neg f   -> f
    Not f   -> f
    Cnv _ f -> f

formatOfBinary :: BinaryOp -> Format
formatOfBinary op = case op of
    Add f -> f
    Sub f -> f
    Mul f -> f
    Div f -> f
    Rem f -> f
    Sla f -> f
    Sra f -> f
    Sru f -> f
    And f -> f
    Ior f -> f
    Xor f -> f
    Ceq f -> f
    Cne f -> f
    Clt f -> f
    Cgt f -> f
    Cle f -> f
    Cge f -> f

formatOfAtom :: (Ord n, Show n) => Map n Local -> Atom n -> Format
formatOfAtom env atom = case atom of
    Var x   -> formatOfLocal (unsafeLookup "formatOfAtom" x env)
    Num _ f -> f
    Str _   -> Fmt A 0

formatsOfTail :: (Ord n, Show n) => Map n Local -> Tail n -> [Format]
formatsOfTail env tl = case tl of
    Copy xs                           -> map (formatOfAtom env) xs
    Invoke t _ _                      -> formatsOfFunType t
    InvokeUnary  op _                 -> [formatOfUnary op]
    InvokeBinary op _ _               -> [formatOfBinary op]
    InvokeVirtual (IMethod _ _ t) _ _ -> formatsOfMethodType t
    InvokeSpecial (IMethod _ _ t) _ _ -> formatsOfMethodType t
    InvokeStatic  (SMethod _ _ t) _   -> formatsOfMethodType t
    GetField      (IField _ _ t)  _   -> [formatOfType t]
    PutField      _               _ _ -> []
    GetStatic     (SField _ _ t)      -> [formatOfType t]
    PutStatic     _               _   -> []

------------------------------------------------------------------------

iMethodRef :: IMethod -> G.MethodRef
iMethodRef (IMethod cls mth ty) = G.MethodRef (G.ClassRef cls) (G.NameType mth (G.Type (describeMethodType ty)))

sMethodRef :: SMethod -> G.MethodRef
sMethodRef (SMethod cls mth ty) = G.MethodRef (G.ClassRef cls) (G.NameType mth (G.Type (describeMethodType ty)))

iFieldRef :: IField -> G.FieldRef
iFieldRef (IField cls fld ty) = G.FieldRef (G.ClassRef cls) (G.NameType fld (G.Type (describeType ty)))

sFieldRef :: SField -> G.FieldRef
sFieldRef (SField cls fld ty) = G.FieldRef (G.ClassRef cls) (G.NameType fld (G.Type (describeType ty)))

------------------------------------------------------------------------

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
