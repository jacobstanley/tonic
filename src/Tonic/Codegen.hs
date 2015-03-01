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

------------------------------------------------------------------------

closureOfBinding :: Show n => n -> Binding n -> G.Class
closureOfBinding n (Lambda ns x) =
    addMethod name mtyp (G.Code 1 8 []) cls
  where
    cls  = mkClass ("clo$" <> name)
    name = T.pack (show n)
    mtyp = MethodType [] Nothing

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

addField :: FieldName -> VarType -> G.Class -> G.Class
addField = addFieldWith [G.F'Private, G.F'Final]

addStaticField :: FieldName -> VarType -> G.Class -> G.Class
addStaticField = addFieldWith [G.F'Private, G.F'Static, G.F'Final]

addMethod :: MethodName -> MethodType -> G.Code -> G.Class -> G.Class
addMethod = addMethodWith [G.M'Public, G.M'Final]

addStaticMethod :: MethodName -> MethodType -> G.Code -> G.Class -> G.Class
addStaticMethod = addMethodWith [G.M'Public, G.M'Static, G.M'Final]

addFieldWith :: [G.FieldAccess] -> FieldName -> VarType -> G.Class -> G.Class
addFieldWith acc name typ cls = cls { G.cFields = G.cFields cls ++ [fld] }
  where
    fld = G.Field acc name (G.Type (describeVarType typ))

addMethodWith :: [G.MethodAccess] -> MethodName -> MethodType -> G.Code -> G.Class -> G.Class
addMethodWith acc name typ code cls = cls { G.cMethods = G.cMethods cls ++ [mth] }
  where
    mth = G.Method acc name (G.Type (describeMethodType typ)) (Just code)

------------------------------------------------------------------------

describeVarType :: VarType -> Text
describeVarType vtyp = case vtyp of
    FunTy ins outs   -> "L" <> mangleFunTy ins outs <> ";"
    NumTy (Fmt U 1)  -> "Z"
    NumTy (Fmt I 8)  -> "B"
    NumTy (Fmt U 16) -> "C"
    NumTy (Fmt I 16) -> "S"
    NumTy (Fmt I 32) -> "I"
    NumTy (Fmt I 64) -> "J"
    NumTy (Fmt F 32) -> "F"
    NumTy (Fmt F 64) -> "D"
    ObjTy name       -> "L" <> name <> ";"
    ArrTy etyp       -> "[" <> describeVarType etyp

describeMethodType :: MethodType -> Text
describeMethodType (MethodType args ret) = "(" <> args' <> ")" <> ret'
  where
    args' = T.concat (map describeVarType args)
    ret'  = maybe "V" describeVarType ret

mangleFunTy :: [VarType] -> [VarType] -> Text
mangleFunTy ins outs = "F" <> mangle ins <> "$$" <> mangle outs
  where
    mangle = T.replace "/" "_"
           . T.replace ";" "_$"
           . T.replace "[" "A"
           . T.concat
           . map describeVarType
