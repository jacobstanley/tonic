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

import           Tonic (foo0, fvOfTerm)

------------------------------------------------------------------------

main :: IO ()
main = do
    print foo0
    print (fvOfTerm foo)

    withFile "Jvmc.class" WriteMode $ \h ->
        hPutBuilder h $ bClass $ jvmc -- (Code 8 8 xs)

------------------------------------------------------------------------

jvmc :: Class
jvmc = Class
    { cAccess     = [ C'Public, C'Final, C'Super ]
    , cName       = ClassRef "Jvmc"
    , cSuper      = ClassRef "java/lang/Object"
    , cInterfaces = []
    , cFields     = [ Field [F'Static] "foo" (Type "I") ]
    , cMethods    = [ Method [M'Public, M'Static] "main" (Type "([Ljava/lang/String;)V") (Just code)
                    --, Method [M'Public, M'Static] "test" (Type "()Ljava/lang/String;") (Just testCode)
                    ]
    , cSourceFile = Just "Jvmc.java"
    }
  where
    code = Code 8 1 [ GetStatic sysOut
                    , SConst "Hello World!"
                    , InvokeVirtual println

                    , GetStatic sysOut
                    , InvokeStatic test
                    , InvokeVirtual println

                    , IConst 42
                    , InvokeStatic sysExit
                    , Return ]

    sysOut  = FieldRef  (ClassRef "java/lang/System")    (NameType "out"     (Type "Ljava/io/PrintStream;"))
    sysExit = MethodRef (ClassRef "java/lang/System")    (NameType "exit"    (Type "(I)V"))
    println = MethodRef (ClassRef "java/io/PrintStream") (NameType "println" (Type "(Ljava/lang/String;)V"))

    test = MethodRef (ClassRef "Jvmc") (NameType "test" (Type "()Ljava/lang/String;"))
