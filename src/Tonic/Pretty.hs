{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tonic.Pretty
    ( printTerm
    , printTail
    ) where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Ratio (numerator, denominator)
import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen

import           Tonic.Types

------------------------------------------------------------------------

printTerm :: Pretty a => Term a -> IO ()
printTerm tm = putDoc (ppTerm tm) >> putStrLn ""

printTail :: Pretty a => Tail a -> IO ()
printTail tl = putDoc (ppTail tl) >> putStrLn ""

------------------------------------------------------------------------

ppAtom :: Pretty n => Atom n -> Doc
ppAtom (Var x)     = var x
ppAtom (Num x fmt) = red (num x) <> dullred (ppFormat fmt)
ppAtom (Str x)     = dullgreen (dquotes (text (T.unpack x)))

ppAtoms :: Pretty n => [Atom n] -> Doc
ppAtoms = tuple . map ppAtom

ppNames :: Pretty n => [n] -> Doc
ppNames = tuple . map letName

ppTail :: Pretty n => Tail n -> Doc
ppTail (Copy xs)            = ppAtoms xs
ppTail (Call f xs)          = ppAtom f <+> ppAtoms xs
ppTail (CallUnary   o x)    = ppUnaryOp o <> ppAtom x
ppTail (CallBinary  o x y)  = ppAtom x <+> ppBinaryOp o <+> ppAtom y
ppTail (CallStatic  m   xs) = kw "callstatic"  <+> ppSMethod m <+> ppAtoms xs
ppTail (CallVirtual m i xs) = kw "callvirtual" <+> ppIMethod m <+> ppAtom i <+> ppAtoms xs
ppTail (CallSpecial m i xs) = kw "callspecial" <+> ppIMethod m <+> ppAtom i <+> ppAtoms xs
ppTail (GetField    f i)    = kw "getfield"    <+> ppIField  f <+> ppAtom i
ppTail (SetField    f i x)  = kw "setfield"    <+> ppIField  f <+> ppAtom i <+> op "<-" <+> ppAtom x
ppTail (GetStatic   f)      = kw "getstatic"   <+> ppSField  f
ppTail (SetStatic   f x)    = kw "setstatic"   <+> ppSField  f <+> op "<-" <+> ppAtom x

ppBinding :: Pretty n => Binding n -> Doc
ppBinding (Lambda ns x) = op "\\" <> ppNames ns <+> op "->" <+> ppTerm x
ppBinding (Const     x) = ppTerm x

ppBindings :: Pretty n => Map n (Binding n) -> Doc
ppBindings = align . vcat . map go . M.toList
  where
    go (n, b) = ppNames [n] <+> op "=" <+> align (ppBinding b)

ppTerm :: Pretty n => Term n -> Doc
ppTerm (Return x)    = ppTail x
ppTerm (Iff i t e)   = kw "if" <+> ppAtom i </> kw "then" <+> ppTerm t
                                            </> kw "else" <+> ppTerm e
ppTerm (Let ns x y)  = kw "let" <+> ppNames ns
                                <+> op "=" <+> align (ppTail x)
                                <+> kw "in" <$> ppTerm y

ppTerm (LetRec bs x) = kw "letrec" <+> ppBindings bs <+> kw "in" <$> ppTerm x

------------------------------------------------------------------------

ppUnaryOp :: UnaryOp -> Doc
ppUnaryOp _ = error "ppUnaryOp"

ppBinaryOp :: BinaryOp -> Doc
ppBinaryOp (Add f) = op "+" <> dullred (ppFormat f)
ppBinaryOp _       = error "ppBinaryOp"

ppFormat :: Format -> Doc
ppFormat (Fmt N sz) = text "`U" <> integer sz
ppFormat (Fmt Z sz) = text "`I" <> integer sz
ppFormat (Fmt R sz) = text "`F" <> integer sz

------------------------------------------------------------------------

ppSMethod :: SMethod -> Doc
ppSMethod (SMethod cls mth _ _) = yellow $ text (T.unpack cls) <> dot <> text (T.unpack mth)

ppIMethod :: IMethod -> Doc
ppIMethod (IMethod cls mth _ _) = yellow $ text (T.unpack cls) <> dot <> text (T.unpack mth)

ppSField :: SField -> Doc
ppSField (SField cls fld _) = yellow $ text (T.unpack cls) <> dot <> text (T.unpack fld)

ppIField :: IField -> Doc
ppIField (IField cls fld _) = yellow $ text (T.unpack cls) <> dot <> text (T.unpack fld)

------------------------------------------------------------------------
-- Utils

num :: Rational -> Doc
num x | denominator x == 1 = pretty (numerator x)
      | otherwise          = pretty (realToFrac x :: Double)

kw :: String -> Doc
kw name = dullblue (text name)

op :: String -> Doc
op name = dullyellow (text name)

var :: Pretty a => a -> Doc
var x = cyan (pretty x)

letName :: Pretty a => a -> Doc
letName x = magenta (pretty x)

tuple :: [Doc] -> Doc
tuple []  = dullyellow (lparen <> rparen)
tuple [x] = x
tuple xs  = parens . hcat $ punctuate (comma <> space) xs
