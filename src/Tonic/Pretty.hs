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
ppAtom atom = case atom of
    Var x     -> var x
    Num x fmt -> red (num x) <> dullred (ppFormat fmt)
    Str x     -> dullgreen (dquotes (text (T.unpack x)))

ppAtoms :: Pretty n => [Atom n] -> Doc
ppAtoms = tuple . map ppAtom

ppNames :: Pretty n => [n] -> Doc
ppNames = tuple . map letName

ppTail :: Pretty n => Tail n -> Doc
ppTail tl = case tl of
    Copy              xs -> ppAtoms xs
    Invoke        _ f xs -> ppAtom f <+> ppAtoms xs
    InvokeUnary   o x    -> ppUnaryOp o <> ppAtom x
    InvokeBinary  o x y  -> ppAtom x <+> ppBinaryOp o <+> ppAtom y
    InvokeStatic  m   xs -> kw "invokestatic"  <+> ppSMethod m <+> ppAtoms xs
    InvokeVirtual m i xs -> kw "invokevirtual" <+> ppVMethod m <+> ppAtom i <+> ppAtoms xs
    InvokeSpecial m i xs -> kw "invokespecial" <+> ppVMethod m <+> ppAtom i <+> ppAtoms xs
    GetField      f i    -> kw "getfield"      <+> ppIField  f <+> ppAtom i
    PutField      f i x  -> kw "putfield"      <+> ppIField  f <+> ppAtom i <+> op "<-" <+> ppAtom x
    GetStatic     f      -> kw "getstatic"     <+> ppSField  f
    PutStatic     f x    -> kw "putstatic"     <+> ppSField  f <+> op "<-" <+> ppAtom x
    New           c   xs -> kw "new"           <+> ppConstructor c <+> ppAtoms xs

ppBinding :: Pretty n => Binding n -> Doc
ppBinding binding = case binding of
    Lambda _ ns x -> op "\\" <> ppNames ns <+> op "->" <+> align (ppTerm x)
    Const  _    x -> align (ppTerm x)

ppBindings :: Pretty n => Map n (Binding n) -> Doc
ppBindings bs | M.null bs = tuple []
              | otherwise = align . vcat . map go $ M.toList bs
  where
    go (n, b) = ppNames [n] <+> op "=" <+> ppBinding b

ppTerm :: Pretty n => Term n -> Doc
ppTerm term = case term of
    Return x    -> ppTail x

    Iff i t e   -> kw "if" <+> ppAtom i <$> kw "then" <+> align (ppTerm t)
                                        <$> kw "else" <+> align (ppTerm e)

    Let ns x y  -> kw "let" <+> ppNames ns
                            <+> op "=" <+> align (ppTail x)
                            <$> ppTerm y

    LetRec bs x -> kw "letrec" <+> ppBindings bs
                               <$> ppTerm x

------------------------------------------------------------------------

ppUnaryOp :: UnaryOp -> Doc
ppUnaryOp o = case o of
    _ -> error "ppUnaryOp"

ppBinaryOp :: BinaryOp -> Doc
ppBinaryOp o = case o of
    Add f -> op "+" <> dullred (ppFormat f)
    Sub f -> op "-" <> dullred (ppFormat f)
    Mul f -> op "*" <> dullred (ppFormat f)
    Div f -> op "/" <> dullred (ppFormat f)
    Ceq f -> op "==" <> dullred (ppFormat f)
    _     -> error "ppBinaryOp"

ppFormat :: Format -> Doc
ppFormat (Fmt U sz) = text "`U" <> integer sz
ppFormat (Fmt I sz) = text "`I" <> integer sz
ppFormat (Fmt F sz) = text "`F" <> integer sz
ppFormat (Fmt A  _) = text "`A"

------------------------------------------------------------------------

ppConstructor :: Constructor -> Doc
ppConstructor (Constructor cls _) = yellow $ text (T.unpack cls)

ppSMethod :: SMethod -> Doc
ppSMethod (SMethod cls mth _) = yellow $ text (T.unpack cls) <> dot <> text (T.unpack mth)

ppVMethod :: VMethod -> Doc
ppVMethod (VMethod cls mth _) = yellow $ text (T.unpack cls) <> dot <> text (T.unpack mth)

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
tuple []  = dullblack (lparen <> rparen)
tuple [x] = x
tuple xs  = dullblack (parens . hcat $ punctuate (comma <> space) xs)
