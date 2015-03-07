{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Tonic.Codegen where

import           Control.Arrow (first)
import           Data.List (zipWith3)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
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

codeOfTail :: (Ord n, Show n) => Map n Local -> Tail n -> [G.Instruction]
codeOfTail env tl = case tl of
    Copy              xs -> map (iPush env) xs
    Invoke       ft f xs -> map (iPush env) (f:xs)
                         <> [G.InvokeInterface (fMethodRef ft) (fromIntegral (length xs + 1))]

    InvokeUnary  op x    -> [iPush env x, iUnary op]
    InvokeBinary op x y  -> [iPush env x, iPush env y, iBinary op]
    InvokeVirtual m i xs -> map (iPush env) (i:xs) <> [G.InvokeVirtual (vMethodRef m)]
    InvokeSpecial m i xs -> map (iPush env) (i:xs) <> [G.InvokeSpecial (vMethodRef m)]
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
codeOfLetRec vars env bs term = (code0 <> code1, concat clsss0 <> clss1)
  where
    bindingNames = M.keys bs
    bindings     = M.elems bs

    (vars', locals) = allocLocals vars (map typeOfBinding bindings)
    env'            = env `mapUnionR` M.fromList (bindingNames `zip` locals)

    scopeTys        = M.map typeOfLocal env `mapUnionR` M.map typeOfBinding bs
    (fldss, clsss0) = unzip (map (closureOfBinding scopeTys . first className) (M.toList bs))

    code0 = concat (zipWith  new        bindingNames locals)
         <> concat (zipWith3 initFields bindingNames locals fldss)

    new n l = [ G.New (G.ClassRef (className n))
              , G.Dup
              , G.InvokeSpecial (ctorRef n)
              , iStore l ]

    initFields n l flds = [ iLoad l ]
                       <> map (nLoad . fst) flds
                       <> [ G.InvokeVirtual (initRef n (map snd flds)) ]

    ctorRef n    = vMethodRef (VMethod (className n) "<init>" (MethodType [] Nothing))
    initRef n ts = vMethodRef (VMethod (className n)  "init"  (MethodType ts Nothing))

    nLoad n = iLoad (unsafeLookup "codeOfLetRec" n env')

    (code1, clss1) = codeOfTerm vars' env' term

    className n = "C$" <> T.replace "\"" "" (T.pack (show n))

allocLocals :: [G.VarIndex] -> [Type] -> ([G.VarIndex], [Local])
allocLocals vs ts = (drop (length ts) vs, zipWith L vs ts)

------------------------------------------------------------------------

closureOfBinding :: (Ord n, Show n) => Map n Type -> (ClassName, Binding n) -> ([(n, Type)], [G.Class])
closureOfBinding scopeTys (clsName, Lambda ftyp@(FunType argTys outTys) argNs term) = (flds, cls : clss)
  where
    flds   = M.toList (scopeTys `mapIntersectionS` fvOfTerm term)
    fldNs  = map fst flds
    fldTys = map snd flds

    assignments = fldNs `zip` map ifld flds
    ifld (n, t) = IField clsName (fieldName n) t

    vars0  = [1..]
    vars1 = drop (length argNs) vars0

    argLocals = zipWith L vars0 argTys

    env = M.fromList (argNs `zip` argLocals)

    (code, clss) = codeOfTerm vars1 env term'
    term' = foldr (.) id (map assignField assignments) term

    assignField (n, f) = Let [n] (GetField f (Num 1 (Fmt A 0)))

    cls = addFields flds
        . addInterface (mangleFunType ftyp)
        . addMethod "invoke" (methodTypeOfFunType ftyp) (G.Code 8 code)
        . addInitMethod flds
        . addEmptyCtor
        $ mkClass clsName

addFields :: Show n => [(n, Type)] -> G.Class -> G.Class
addFields = foldr (.) id . map (uncurry addField)

addField :: Show n => n -> Type -> G.Class -> G.Class
addField n t = addFieldWith [G.F'Private, G.F'Final] (fieldName n) t

fieldName :: Show n => n -> FieldName
fieldName n = "_" <> T.replace "\"" "" (T.pack (show n))

addInitMethod :: Show n => [(n, Type)] -> G.Class -> G.Class
addInitMethod flds cls =
    addMethodWith [G.M'Public, G.M'Final] "init"
                  (MethodType (map snd flds) Nothing)
                  (G.Code 8 code) cls
  where
    code = concat (zipWith putField [1..] flds) <> [G.Return]

    putField i (n, t) = [ G.ALoad 0
                        , iLoad (L i t)
                        , G.PutField (fldRef n t) ]

    fldRef n t = iFieldRef (IField clsName (fieldName n) t)
    clsName    = G.unClassRef (G.cName cls)

addEmptyCtor :: G.Class -> G.Class
addEmptyCtor cls =
    addMethodWith [G.M'Public] "<init>"
                  (MethodType [] Nothing)
                  (G.Code 8 code) cls
  where
    code = [G.ALoad 0, G.InvokeSpecial (vMethodRef objInit), G.Return]
    objInit = VMethod "java/lang/Object" "<init>" (MethodType [] Nothing)

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
    InvokeVirtual (VMethod _ _ t) _ _ -> outputsOfMethodType t
    InvokeSpecial (VMethod _ _ t) _ _ -> outputsOfMethodType t
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

fInterfaceRef :: FunType -> G.ClassRef
fInterfaceRef = G.ClassRef . mangleFunType

fMethodRef :: FunType -> G.IMethodRef
fMethodRef ft = G.IMethodRef (G.ClassRef cls) (G.NameType "invoke" (G.Type desc))
  where
    cls  = mangleFunType ft
    mt   = methodTypeOfFunType ft
    desc = describeMethodType mt

vMethodRef :: VMethod -> G.MethodRef
vMethodRef (VMethod cls mth ty) = G.MethodRef (G.ClassRef cls) (G.NameType mth (G.Type (describeMethodType ty)))

sMethodRef :: SMethod -> G.MethodRef
sMethodRef (SMethod cls mth ty) = G.MethodRef (G.ClassRef cls) (G.NameType mth (G.Type (describeMethodType ty)))

iMethodRef :: IMethod -> G.IMethodRef
iMethodRef (IMethod cls mth ty) = G.IMethodRef (G.ClassRef cls) (G.NameType mth (G.Type (describeMethodType ty)))

iFieldRef :: IField -> G.FieldRef
iFieldRef (IField cls fld ty) = G.FieldRef (G.ClassRef cls) (G.NameType fld (G.Type (describeType ty)))

sFieldRef :: SField -> G.FieldRef
sFieldRef (SField cls fld ty) = G.FieldRef (G.ClassRef cls) (G.NameType fld (G.Type (describeType ty)))

------------------------------------------------------------------------

methodTypeOfFunType :: FunType -> MethodType
methodTypeOfFunType ft@(FunType argTys outTys) = case outTys of
    []    -> MethodType argTys Nothing
    [typ] -> MethodType argTys (Just typ)
    _     -> error $ "methodTypeOfFunType: multiple return "
                  <> "values not supported: " <> show ft

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
mangleFunType (FunType args outs) = "G" <> mangle args <> "$$" <> mangle outs
  where
    mangle = T.replace "/" "_"
           . T.replace ";" "_"
           . T.replace "[" "A"
           . T.concat
           . map describeType
