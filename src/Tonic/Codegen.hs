{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Tonic.Codegen where

import           Control.Arrow (first)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe (maybeToList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import qualified JVM.Codegen as G

import           Tonic
import           Tonic.Types
import           Tonic.Utils

------------------------------------------------------------------------

data Local = L G.VarIndex Type
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

interfacesOfBinding :: Binding n -> Set G.Class
interfacesOfBinding (Const  _   x) = interfacesOfTerm x
interfacesOfBinding (Lambda t _ x) = interfacesOfTerm x `S.union` S.singleton iface
  where
    iface = (mkInterface $ mangleFunType t) {
              G.cMethods = [G.Method acc "invoke" mtyp Nothing]
            }

    acc  = [G.M'Public, G.M'Abstract]
    mtyp = G.Type (describeMethodType (methodTypeOfFunType t))

interfacesOfTerm :: Term n -> Set G.Class
interfacesOfTerm term = case term of
    Return    _ -> S.empty
    Iff   _ t e -> interfacesOfTerm t <> interfacesOfTerm e
    Let   _ _ x -> interfacesOfTerm x
    LetRec bs x -> S.unions (map interfacesOfBinding (M.elems bs)) <> interfacesOfTerm x

------------------------------------------------------------------------

closureOfBinding :: (Ord n, Show n) => Map n Type -> ClassName -> Binding n -> (G.Class, [G.Class])
closureOfBinding scopeTys name (Lambda ftyp@(FunType argTys outTys) argNs term) = (cls, clss)
  where
    flds   = M.toList (scopeTys `mapIntersectionS` fvOfTerm term)
    fldNs  = map fst flds
    fldTys = map snd flds

    vars0  = [1..]
    vars1 = drop (length argNs) vars0
    vars2 = drop (length fldNs) vars1

    argLocals = zipWith L vars0 argTys
    fldLocals = zipWith L vars1 fldTys

    env = M.fromList (argNs `zip` argLocals) `mapUnionR`
          M.fromList (fldNs `zip` fldLocals)

    (code, clss) = codeOfTerm vars2 env term'
    term' = foldr (.) id (map assignField flds) term

    assignField (n, t) =
        Let [n] (GetField fld (Num 1 (Fmt A 0)))
      where
        fld = IField clsName (fieldName n) t

    cls = addFields flds
        . addInterface (mangleFunType ftyp)
        . addMethod "invoke" (methodTypeOfFunType ftyp) (G.Code 8 code)
        . addEmptyConstructor
        $ mkClass clsName

    clsName = "C$" <> name

methodTypeOfFunType :: FunType -> MethodType
methodTypeOfFunType ft@(FunType argTys outTys) = case outTys of
    []    -> MethodType argTys Nothing
    [typ] -> MethodType argTys (Just typ)
    _     -> error $ "methodTypeOfFunType: multiple return "
                  <> "values not supported: " <> show ft

addFields :: Show n => [(n, Type)] -> G.Class -> G.Class
addFields = foldr (.) id . map (uncurry addField)

addField :: Show n => n -> Type -> G.Class -> G.Class
addField n t = addFieldWith [G.F'Private] (fieldName n) t

fieldName :: Show n => n -> FieldName
fieldName n = "_" <> T.replace "\"" "" (T.pack (show n))

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
    New           c   xs -> [G.New (cClassRef c), G.Dup]
                         <> map (iPush env) xs
                         <> [G.InvokeSpecial (cMethodRef c)]

codeOfTerm :: (Ord n, Show n) => [G.VarIndex] -> Map n Local -> Term n -> ([G.Instruction], [G.Class])
codeOfTerm vars env term = case term of
    Return x    -> let code0 = codeOfTail env x
                       code1 = case typesOfTail env x of
                         []    -> [G.Return]
                         [typ] -> [iReturn typ]
                         _     -> error $ "codeOfTerm: multiple return "
                                       <> "values not supported: " <> show x
                   in
                       (code0 <> code1, [])

    Let ns x y  -> let code0       = codeOfTail env  x

                       (vars', ls) = allocLocals vars (typesOfTail env x)
                       env'        = env `mapUnionR` M.fromList (ns `zip` ls)
                       code1       = map iStore ls

                       (code2, cs) = codeOfTerm vars' env' y
                   in
                       (code0 <> code1 <> code2, cs)

    LetRec bs x -> codeOfLetRec vars env bs x

    _ -> error ("codeOfTerm: unsupported: " <> show term)


codeOfLetRec :: (Ord n, Show n) => [G.VarIndex] -> Map n Local -> Bindings n -> Term n -> ([G.Instruction], [G.Class])
codeOfLetRec vars env bs term = (code0 <> code1, clss0 <> clss1)
  where
    (vars', ls) = allocLocals vars (map typeOfBinding (M.elems bs))
    env'        = env `mapUnionR` M.fromList (M.keys bs `zip` ls)
    code0       = concat $ zipWith (\xs y -> xs ++ [y])
                                   (map (codeOfTail env) news)
                                   (map iStore ls)

    news = map new (M.toList bs)
    new (n, b) = New (ctor n) []

    className = T.replace "\"" "" . T.pack . show
    ctor n    = Constructor ("C$" <> className n) (MethodType [] Nothing)

    scopeTys  = M.map typeOfLocal env `mapUnionR` M.map typeOfBinding bs
    closure   = uncurry (:) . uncurry (closureOfBinding scopeTys)
    clss0     = concatMap (closure . first className) (M.toList bs)

    (code1, clss1) = codeOfTerm vars' env' term


allocLocals :: [G.VarIndex] -> [Type] -> ([G.VarIndex], [Local])
allocLocals vs ts = (drop (length ts) vs, zipWith L vs ts)

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
iConst 1 (Fmt A _) = G.ALoad 0
iConst x fmt       = case fmt of
    Fmt I s | s <= 32 -> G.IConst (truncate x)
            | s <= 64 -> G.LConst (truncate x)
    Fmt F s | s <= 32 -> G.FConst (fromRational x)
            | s <= 64 -> G.DConst (fromRational x)
    _                 -> error ("iConst: cannot load constant: " <> show x <> " (with format: " <> show fmt <> ")")

iLoad :: Local -> G.Instruction
iLoad (L idx typ) = case formatOfType typ of
    Fmt A _           -> G.ALoad idx
    Fmt I s | s <= 32 -> G.ILoad idx
            | s <= 64 -> G.LLoad idx
    Fmt F s | s <= 32 -> G.FLoad idx
            | s <= 64 -> G.DLoad idx
    fmt               -> error ("iLoad: cannot load format: " <> show fmt)

iStore :: Local -> G.Instruction
iStore (L idx typ) = case formatOfType typ of
    Fmt A _           -> G.AStore idx
    Fmt I s | s <= 32 -> G.IStore idx
            | s <= 64 -> G.LStore idx
    Fmt F s | s <= 32 -> G.FStore idx
            | s <= 64 -> G.DStore idx
    fmt               -> error ("iStore: cannot store format: " <> show fmt)

iReturn :: Type -> G.Instruction
iReturn typ = case formatOfType typ of
    Fmt U s | s <= 32 -> G.IReturn
            | s <= 64 -> G.LReturn
    Fmt I s | s <= 32 -> G.IReturn
            | s <= 64 -> G.LReturn
    Fmt F s | s <= 32 -> G.FReturn
            | s <= 64 -> G.DReturn
    Fmt A _           -> G.AReturn

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

mkInterface :: ClassName -> G.Class
mkInterface name = (mkClass name) { G.cAccess = [ G.C'Public, G.C'Interface, G.C'Abstract ] }

addInterface :: ClassName -> G.Class -> G.Class
addInterface name cls = cls { G.cInterfaces = G.cInterfaces cls ++ [G.ClassRef name] }

addEmptyConstructor :: G.Class -> G.Class
addEmptyConstructor =
    addMethodWith [G.M'Public] "<init>" (MethodType [] Nothing) (G.Code 8 code)
  where
    code = [ G.ALoad 0
           , G.InvokeSpecial (cMethodRef ctor)
           , G.Return ]

    ctor = Constructor "java/lang/Object" (MethodType [] Nothing)

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

typeOfLocal :: Local -> Type
typeOfLocal (L _ t) = t

outputsOfFunType :: FunType -> [Type]
outputsOfFunType (FunType _ os) = os

outputsOfMethodType :: MethodType -> [Type]
outputsOfMethodType (MethodType _ os) = maybeToList os

typeOfUnary :: UnaryOp -> Type
typeOfUnary op = NumTy $ case op of
    Neg f   -> f
    Not f   -> f
    Cnv _ f -> f

typeOfBinary :: BinaryOp -> Type
typeOfBinary op = NumTy $ case op of
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

typeOfAtom :: (Ord n, Show n) => Map n Local -> Atom n -> Type
typeOfAtom env atom = case atom of
    Var x   -> typeOfLocal (unsafeLookup "typeOfAtom" x env)
    Num _ f -> NumTy f
    Str _   -> ObjTy "java/lang/String"

typesOfTail :: (Ord n, Show n) => Map n Local -> Tail n -> [Type]
typesOfTail env tl = case tl of
    Copy xs                           -> map (typeOfAtom env) xs
    Invoke t _ _                      -> outputsOfFunType t
    InvokeUnary  op _                 -> [typeOfUnary op]
    InvokeBinary op _ _               -> [typeOfBinary op]
    InvokeVirtual (IMethod _ _ t) _ _ -> outputsOfMethodType t
    InvokeSpecial (IMethod _ _ t) _ _ -> outputsOfMethodType t
    InvokeStatic  (SMethod _ _ t) _   -> outputsOfMethodType t
    GetField      (IField _ _ t)  _   -> [t]
    PutField      _               _ _ -> []
    GetStatic     (SField _ _ t)      -> [t]
    PutStatic     _               _   -> []
    New           (Constructor c _) _ -> [ObjTy c]

typeOfBinding :: Binding n -> Type
typeOfBinding (Lambda t _ _) = FunTy t
typeOfBinding (Const  t _)   = t

------------------------------------------------------------------------

cClassRef :: Constructor -> G.ClassRef
cClassRef (Constructor cls _) = G.ClassRef cls

cMethodRef :: Constructor -> G.MethodRef
cMethodRef (Constructor cls ty) = G.MethodRef (G.ClassRef cls) (G.NameType "<init>" (G.Type (describeMethodType ty)))

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
mangleFunType (FunType ins outs) = "G" <> mangle ins <> "$$" <> mangle outs
  where
    mangle = T.replace "/" "_"
           . T.replace ";" "_$"
           . T.replace "[" "A"
           . T.concat
           . map describeType
