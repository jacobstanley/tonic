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
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>), mconcat)
import           Data.Text (Text)
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
    -- print foo2
    -- print (fvOfTerm foo2)
    -- print (rename $ simplifyTerm foo2)
    print (foo1)
    print (deadTerm foo1)
    print (rename $ deadTerm foo1)

    withFile "Jvmc.class" WriteMode $ \h ->
        hPutBuilder h $ bClass $ jvmc -- (Code 8 8 xs)

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
    code = Code 8 1 [ G.GetStatic sysOut
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
