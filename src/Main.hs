{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -w #-}

module Main (main) where

import           Control.Applicative (Applicative, (<$>), (<*>), pure)
import           Control.Monad.State.Lazy
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List (foldl1')
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Maybe (mapMaybe, fromJust)
import           Data.Monoid ((<>), mconcat)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word (Word8, Word16)
import           System.IO (IOMode(..), withFile)

import qualified JVM.Codegen as G
import           JVM.Codegen hiding (Instruction(..))
import           Tonic
import           Tonic.Codegen
import           Tonic.Pretty
import           Tonic.Types

import qualified Text.PrettyPrint.ANSI.Leijen as PP

------------------------------------------------------------------------

main :: IO ()
main = do
    --let lam = Lambda (FunType [] [NF32]) [] $
    --          Let ["r"] (InvokeBinary (Add F32) (Var "x") (Num 1 F32)) $
    --          Return (Copy [Var "r"])

    --let (cls, clss) = closureOfBinding (M.fromList [("x", NumTy F32)]) "f" lam
    --mapM_ (writeClass "output") (cls:clss)

    writeMain (rename oddeven)

------------------------------------------------------------------------

pattern F32     = Fmt F 32
pattern I32     = Fmt I 32
pattern U1      = Fmt U 1
pattern NF32    = NumTy F32
pattern NI32    = NumTy I32
pattern NU1     = NumTy U1
pattern JString = ObjTy "java/lang/String"

foo0 :: Term String
foo0 =
    Let [c] (Copy [Num 42 F32]) $
    Let [x] (InvokeBinary (Add F32) (Var c) (Var c)) $
    Let [z] (InvokeBinary (Mul F32) (Var c) (Var c)) $
    Let [w] (InvokeBinary (Sub F32) (Var z) (Var x)) $
    Let [y] (InvokeStatic float2string [Var w]) $
    Let [out] (GetStatic sysOut) $
    Return (InvokeVirtual println (Var out) [Var y])
  where
    (c,x,y,z,w,out) = ("c","x","y","z","w","out")

foo1 :: Term String
foo1 =
    Let [x] (Copy [Num 9 F32]) $
    Let [y] (Copy [Num 1 F32]) $
    Let [w] (Copy [Num 42 F32]) $
    Let [z] (InvokeBinary (Add F32) (Var x) (Var y)) $

    letrec [ (u, Lambda (FunType [] [NF32]) [] $
                    Let [z] (Copy [Var t]) $
                    Return (Copy [Num 42 F32]) )

           , (t, Lambda (FunType [NF32, NF32] [NF32]) [x, y] $
                    Let [s] (Invoke (FunType [] [NF32]) (Var u) []) $
                    Let [z] (InvokeBinary (Add F32) (Var x) (Var y)) $
                    Let [w] (InvokeBinary (Add F32) (Var s) (Var z)) $
                    Return (Copy [Var w])) ] $
                    -- Return (InvokeStatic float2string [Var w])) ] $

    Let [s] (Invoke (FunType [NF32, NF32] [NF32]) (Var t) [Var x, Var w]) $
    Let [s] (InvokeStatic float2string [Var s]) $
    Let [out] (GetStatic sysOut) $
    Return (InvokeVirtual println (Var out) [Var s])
  where
    (x,y,z,w,u,t,s,out) = ("x","y","z","w","u","t","s","out")

    letrec bs tm = LetRec (M.fromList bs) tm

foo2 :: Term String
foo2 =
    letrec [ (toStr, Lambda f2s  [n]   (Return (InvokeStatic float2string [Var n])))
           , (x,     Const  NF32       (Return (Copy [Num 9 F32])))
           , (y,     Const  NF32       (Return (Copy [Num 1 F32])))
           , (add,   Lambda ff2f [x,y] (Return (InvokeBinary (Add F32) (Var x) (Var y))))
           , (f,     Lambda f2f  [w]   (Return (Copy [Var x])))
           , (g,     Lambda _2f  []    (Return (Copy [Var y]))) ] $
    Let [z] (Invoke ff2f (Var add) [Var x, Var y]) $
    Return (Invoke f2s (Var toStr) [Var z])
  where
    (x,y,z,w,f,g,n,add,toStr) = ("x","y","z","w","f","g","n","add","toStr")

    ff2f = FunType [NF32, NF32] [NF32]
    f2f  = FunType [NF32]       [NF32]
    _2f  = FunType []           [NF32]
    f2s  = FunType [NF32]       [JString]

    letrec bs t = LetRec (M.fromList bs) t

oddeven :: Term String
oddeven =
    letrec [ ("odd", Lambda i2b ["x"] $
                        Let ["b"] (InvokeBinary (Ceq I32) (Var "x") zero) $
                        Iff (Var "b") (Return (Copy [false])) $
                        Let ["y"] (InvokeBinary (Sub I32) (Var "x") one) $
                        Return (Invoke i2b (Var "even") [Var "y"]))

           , ("even", Lambda i2b ["x"] $
                        Let ["b"] (InvokeBinary (Ceq I32) (Var "x") zero) $
                        Iff (Var "b") (Return (Copy [true])) $
                        Let ["y"] (InvokeBinary (Sub I32) (Var "x") one) $
                        Return (Invoke i2b (Var "odd") [Var "y"])) ] $

    Return (Invoke i2b (Var "odd") [Num 19 I32])
  where
    zero  = Num 0 I32
    one   = Num 1 I32
    false = Num 0 U1
    true  = Num 1 U1

    i2b  = FunType [NI32] [NU1]

    letrec bs t = LetRec (M.fromList bs) t

float2string :: SMethod
float2string = SMethod "java/lang/Float" "toString" (MethodType [NF32] (Just JString))

sysOut :: SField
sysOut = SField  "java/lang/System" "out" (ObjTy "java/io/PrintStream")

println :: VMethod
println = VMethod "java/io/PrintStream" "println" (MethodType [ObjTy "java/lang/String"] Nothing)

------------------------------------------------------------------------

writeMain :: (Ord n, Show n, PP.Pretty n) => Term n -> IO ()
writeMain term = do
    printTerm term
    mapM_ (writeClass "output") (cls:clss ++ ifcs)
  where
    vars = [1..] -- start from 1 to skip (string[] args)
    (code, clss) = codeOfTerm vars M.empty term

    ifcs = S.toList (interfacesOfTerm term)

    cls = addStaticMethod "main"
            (MethodType [ArrTy (ObjTy "java/lang/String")] Nothing)
            (Code 8 code)
            (mkClass "Main")

writeClass :: FilePath -> G.Class -> IO ()
writeClass dir cls = do
    print cls
    withFile (dir <> "/" <> name <> ".class") WriteMode
             (\h -> hPutBuilder h (bClass cls))
  where
    name = T.unpack (unClassRef (cName cls))

------------------------------------------------------------------------

newtype Var = V Int
    deriving (Eq, Ord, Enum)

instance PP.Pretty Var where
    pretty (V x) = PP.text "x" PP.<> PP.int x

instance Show Var where
    show (V x) = "x" <> show x

rename :: (Ord n, Show n) => Term n -> Term Var
rename = snd . renameTerm [V 1..] M.empty

------------------------------------------------------------------------

jvmc :: Class
jvmc = Class
    { cAccess     = [ C'Public, C'Final, C'Super ]
    , cName       = ClassRef "Jvmc"
    , cSuper      = ClassRef "java/lang/Object"
    , cInterfaces = []
    , cFields     = [ Field [F'Static] "foo" (Type "Ljava/lang/Object;") ]
    , cMethods    = [ Method [M'Public, M'Static] "main" (Type "([Ljava/lang/String;)V") (Just code)
                    --, Method [M'Public, M'Static] "test" (Type "()Ljava/lang/String;") (Just testCode)
                    ]
    , cSourceFile = Just "Jvmc.java"
    }
  where
    code = Code 8 [ G.GetStatic sysOut
                  , G.PutStatic foo

                  , G.GetStatic foo
                  , G.CheckCast printStream
                  , G.SConst "Hello World!"
                  , G.InvokeVirtual println

                  , G.GetStatic foo
                  , G.CheckCast printStream
                  , G.SConst "Goodbye World!"
                  , G.InvokeVirtual println

                  , G.IConst 42
                  , G.InvokeStatic sysExit
                  , G.Return ]

    printStream = ClassRef "java/io/PrintStream"
    sysOut  = FieldRef  (ClassRef "java/lang/System")    (NameType "out"     (Type "Ljava/io/PrintStream;"))
    sysExit = MethodRef (ClassRef "java/lang/System")    (NameType "exit"    (Type "(I)V"))
    println = MethodRef (ClassRef "java/io/PrintStream") (NameType "println" (Type "(Ljava/lang/String;)V"))

    foo = FieldRef (ClassRef "Jvmc") (NameType "foo" (Type "Ljava/lang/Object;"))
