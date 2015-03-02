{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Maybe (mapMaybe, fromJust)
import           Data.Monoid ((<>), mconcat)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word (Word8, Word16)
import           System.IO (IOMode(..), withFile)

import qualified JVM.Codegen as G
import           JVM.Codegen hiding (Instruction(..))
import           Tonic (foo0, foo1, foo2)
import           Tonic
import           Tonic.Codegen
import           Tonic.Pretty
import           Tonic.Types

import qualified Text.PrettyPrint.ANSI.Leijen as PP

------------------------------------------------------------------------

main :: IO ()
main = do
    writeMain foo0

------------------------------------------------------------------------

writeMain :: (Ord n, Show n, PP.Pretty n) => Term n -> IO ()
writeMain term = do
    printTerm term
    mapM_ (writeClass "output") (cls:clss)
  where
    vars = [1..] -- start from 1 to skip (string[] args)
    (code, clss) = codeOfTerm vars M.empty term

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
