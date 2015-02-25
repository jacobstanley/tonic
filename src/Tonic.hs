{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Tonic where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)

------------------------------------------------------------------------

pattern F32 = Fmt R 32

foo0 :: Term Int
foo0 =
    Let [x] (CallBinary (Add F32) (Num 42 F32) (Num 18 F32)) $
    Return (CallStatic float2string [Var x])
  where
    x = 1

foo1 :: Term Int
foo1 =
    Let [x] (Copy [Num 9 F32]) $
    Let [y] (Copy [Num 1 F32]) $
    Let [z] (CallBinary (Add F32) (Var x) (Var y)) $
    Return (CallStatic float2string [Var z])
  where
    (x,y,z) = (1,2,3)

foo2 :: Term String
foo2 =
    letrec [ (x, Lambda []  (Return (Copy [Num 9 F32])))
           , (y, Const      (Return (Copy [Num 1 F32])))
           , (f, Lambda [w] (Return (Copy [Var x])))
           , (g, Lambda []  (Return (Copy [Var y]))) ] $
    Let [z] (CallBinary (Add F32) (Var x) (Var y)) $
    Return (CallStatic float2string [Var z])
  where
    (x,y,z,w,f,g) = ("x","y","z","w","f","g")

    letrec bs t = LetRec (M.fromList bs) t

float2string :: SMethod
float2string = SMethod "java/lang/Float" "toString" [Float] (Just (Object "java/lang/String"))

------------------------------------------------------------------------

data Atom n =
      Var n
    | Num Rational Format
    | Str Text
    deriving (Eq, Ord, Show)

data Tail n =
      Copy [Atom n]

    | Call        (Atom n) [Atom n]
    | CallUnary   UnaryOp  (Atom n)
    | CallBinary  BinaryOp (Atom n) (Atom n)
    | CallVirtual IMethod  (Atom n) [Atom n]
    | CallSpecial IMethod  (Atom n) [Atom n]
    | CallStatic  SMethod  [Atom n]

    | GetField  IField (Atom n)
    | SetField  IField (Atom n) (Atom n)
    | GetStatic SField
    | SetStatic SField (Atom n)
    deriving (Eq, Ord, Show)

type Bindings n = Map n (Binding n)

data Binding n =
      Lambda [n] (Term n)
    | Const      (Term n)
    deriving (Eq, Ord, Show)

data Term n =
      Return (Tail n)
    | Iff    (Atom n) (Term n) (Term n)
    | Let    [n] (Tail n) (Term n)
    | LetRec (Bindings n) (Term n)
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

data UnaryOp =
      Neg Format        -- ^ negation
    | Not Format        -- ^ logical NOT
    | Cnv Format Format -- ^ convert from/to format
    deriving (Eq, Ord, Show)

data BinaryOp =
      Add Format -- ^ addition
    | Sub Format -- ^ subtraction
    | Mul Format -- ^ multiplication
    | Div Format -- ^ division
    | Rem Format -- ^ remainder

    | Sla Format -- ^ shift left  (arithmetic)
    | Sra Format -- ^ shift right (arithmetic)
    | Sru Format -- ^ shift right (unsigned / logical)

    | And Format -- ^ bitwise AND
    | Ior Format -- ^ bitwise Inclusive OR
    | Xor Format -- ^ bitwise eXclusive OR

    | Ceq Format -- ^ equal
    | Cne Format -- ^ not equal
    | Clt Format -- ^ less than
    | Cgt Format -- ^ greater than
    | Cle Format -- ^ less or equal
    | Cge Format -- ^ greater or equal
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------

data Format = Fmt Genre Size
    deriving (Eq, Ord, Show)

data Genre =
      N -- ^ natural numbers
    | Z -- ^ integers
    | R -- ^ rationals
    deriving (Eq, Ord, Enum, Show)

type Size = Integer

------------------------------------------------------------------------

-- | JVM types.
data JType =
      Boolean
    | Byte
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double
    | Object Text
    deriving (Eq, Ord, Show)

type ClassName  = Text
type MethodName = Text
type FieldName  = Text

-- | Instance method.
data IMethod = IMethod ClassName MethodName [JType] (Maybe JType)
    deriving (Eq, Ord, Show)

-- | Static method.
data SMethod = SMethod ClassName MethodName [JType] (Maybe JType)
    deriving (Eq, Ord, Show)

-- | Instance field.
data IField = IField ClassName FieldName JType
    deriving (Eq, Ord, Show)

-- | Static field.
data SField = SField ClassName FieldName JType
    deriving (Eq, Ord, Show)

------------------------------------------------------------------------
-- Finding Free Variables

fvOfAtom :: Ord n => Atom n -> Set n
fvOfAtom atom = case atom of
    Var x   -> S.singleton x
    Num _ _ -> S.empty
    Str _   -> S.empty

fvOfAtoms :: Ord n => [Atom n] -> Set n
fvOfAtoms = S.unions . map fvOfAtom

fvOfTail :: Ord n => Tail n -> Set n
fvOfTail tl = case tl of
    Copy xs            -> fvOfAtoms xs
    Call f xs          -> fvOfAtom f `S.union` fvOfAtoms xs
    CallUnary   _ x    -> fvOfAtom x
    CallBinary  _ x y  -> fvOfAtom x `S.union` fvOfAtom y
    CallVirtual _ i xs -> fvOfAtom i `S.union` fvOfAtoms xs
    CallSpecial _ i xs -> fvOfAtom i `S.union` fvOfAtoms xs
    CallStatic  _   xs -> fvOfAtoms xs
    GetField    _ i    -> fvOfAtom i
    SetField    _ i x  -> fvOfAtom i `S.union` fvOfAtom x
    GetStatic   _      -> S.empty
    SetStatic   _   x  -> fvOfAtom x

fvOfBinding :: Ord n => Binding n -> Set n
fvOfBinding binding = case binding of
    Lambda ns x -> fvOfTerm x `setDifferenceL` ns
    Const     x -> fvOfTerm x

fvOfBindings :: Ord n => Bindings n -> Set n
fvOfBindings = S.unions . map fvOfBinding . M.elems

bvOfBindings :: Ord n => Bindings n -> Set n
bvOfBindings = M.keysSet

fvOfTerm :: Ord n => Term n -> Set n
fvOfTerm term = case term of
    Return x      -> fvOfTail x
    Iff    i t e  -> fvOfAtom i `S.union` fvOfTerm t `S.union` fvOfTerm e
    Let    ns x y -> fvOfTail x `S.union` (fvOfTerm y `setDifferenceL` ns)
    LetRec bs x   -> (fvOfBindings bs `S.union` fvOfTerm x) `S.difference` bvOfBindings bs

------------------------------------------------------------------------
-- Renaming

renameAtom :: (Ord a, Show a, Show b) => Map a b -> Atom a -> Atom b
renameAtom names atom = case atom of
    Var x   -> Var (unsafeLookup "renameAtom: name not found" x names)
    Num x f -> Num x f
    Str x   -> Str x

renameAtoms :: (Ord a, Show a, Show b) => Map a b -> [Atom a] -> [Atom b]
renameAtoms names = map (renameAtom names)

renameTail :: (Ord a, Show a, Show b) => Map a b -> Tail a -> Tail b
renameTail names tl = case tl of
    Copy xs            -> Copy (renameAtoms names xs)
    Call f xs          -> Call (renameAtom names f) (renameAtoms names xs)
    CallUnary   o x    -> CallUnary   o (renameAtom names x)
    CallBinary  o x y  -> CallBinary  o (renameAtom names x) (renameAtom names y)
    CallVirtual m i xs -> CallVirtual m (renameAtom names i) (renameAtoms names xs)
    CallSpecial m i xs -> CallSpecial m (renameAtom names i) (renameAtoms names xs)
    CallStatic  m   xs -> CallStatic  m                      (renameAtoms names xs)
    GetField    f i    -> GetField    f (renameAtom names i)
    SetField    f i x  -> SetField    f (renameAtom names i) (renameAtom names x)
    GetStatic   f      -> GetStatic   f
    SetStatic   f   x  -> SetStatic   f (renameAtom names x)

renameBindings :: (Ord a, Ord b, Show a, Show b) => [b] -> Map a b -> Bindings a -> ([b], Map a b, Bindings b)
renameBindings gen names as = (gen2, names', bs)
  where
    gen1       = drop (M.size as) gen
    names'     = names // M.fromList (M.keys as `zip` gen)
    (gen2, bs) = renameBindingsList gen1 names' (M.toList as)

renameBindingsList :: (Ord a, Ord b, Show a, Show b) => [b] -> Map a b -> [(a, Binding a)] -> ([b], Bindings b)
renameBindingsList gen names bindings = case bindings of

    [(k, Lambda ns x)] -> let (ns', gen1) = splitAt (length ns) gen
                              names'      = names // M.fromList (ns `zip` gen)
                              (gen2, x')  = renameTerm gen1 names' x
                              k'          = unsafeLookup "renameBindingsList: name not found" k names
                          in
                              (gen2, M.singleton k' (Lambda ns' x'))

    [(k, Const x)]     -> let (gen', x') = renameTerm gen names x
                              k'         = unsafeLookup "renameBindingsList: name not found" k names
                          in
                              (gen', M.singleton k' (Const x'))

    []     -> (gen, M.empty)

    (b:bs) -> let (gen1, bs1) = renameBindingsList gen  names [b]
                  (gen2, bs2) = renameBindingsList gen1 names bs
              in
                  (gen2, bs1 `M.union` bs2)


renameTerm :: (Ord a, Ord b, Show a, Show b) => [b] -> Map a b -> Term a -> ([b], Term b)
renameTerm gen names term = case term of
    Return x    -> (gen, Return (renameTail names x))

    Iff i t e   -> let (gen1, t') = renameTerm gen  names t
                       (gen2, e') = renameTerm gen1 names e
                   in
                       (gen2, Iff (renameAtom names i) t' e')

    Let ns x y  -> let (ns', gen1) = splitAt (length ns) gen
                       names'      = names // M.fromList (ns `zip` gen)
                       (gen2, y')  = renameTerm gen1 names' y
                   in
                       (gen2, Let ns' (renameTail names x) y')

    LetRec bs x -> let (gen1, names', bs') = renameBindings gen names bs
                       (gen2, x')          = renameTerm gen1 names' x
                   in
                       (gen2, LetRec bs' x')

------------------------------------------------------------------------
-- Capture Avoiding Substitution

substAtom :: Ord n => Map n (Atom n) -> Atom n -> Atom n
substAtom subs atom = case atom of
    Var x -> fromMaybe atom (M.lookup x subs)
    _     -> atom

substAtoms :: Ord n => Map n (Atom n) -> [Atom n] -> [Atom n]
substAtoms subs = map (substAtom subs)

substTail :: Ord n => Map n (Atom n) -> Tail n -> Tail n
substTail subs tl = case tl of
    Copy xs            -> Copy (substAtoms subs xs)
    Call f xs          -> Call (substAtom subs f) (substAtoms subs xs)
    CallUnary   o x    -> CallUnary   o (substAtom subs x)
    CallBinary  o x y  -> CallBinary  o (substAtom subs x) (substAtom subs y)
    CallVirtual m i xs -> CallVirtual m (substAtom subs i) (substAtoms subs xs)
    CallSpecial m i xs -> CallSpecial m (substAtom subs i) (substAtoms subs xs)
    CallStatic  m   xs -> CallStatic  m                    (substAtoms subs xs)
    GetField    f i    -> GetField    f (substAtom subs i)
    SetField    f i x  -> SetField    f (substAtom subs i) (substAtom subs x)
    GetStatic   f      -> GetStatic   f
    SetStatic   f   x  -> SetStatic   f (substAtom subs x)

substBinding :: Ord n => Map n (Atom n) -> Binding n -> Binding n
substBinding subs binding = case binding of
    Lambda ns x -> Lambda ns (substTerm (subs `mapDifferenceL` ns) x)
    Const     x -> Const     (substTerm subs x)

substTerm :: Ord n => Map n (Atom n) -> Term n -> Term n
substTerm subs term = case term of
    Return x      -> Return (substTail subs x)
    Iff    i t e  -> Iff (substAtom subs i) (substTerm subs t) (substTerm subs e)
    Let    ns x y -> Let ns (substTail subs x) (substTerm (subs `mapDifferenceL` ns) y)
    LetRec bs x   -> let subs' = subs `mapDifferenceS` bvOfBindings bs
                     in  LetRec (M.map (substBinding subs') bs) (substTerm subs' x)

------------------------------------------------------------------------
-- Utils

setDifferenceL :: Ord a => Set a -> [a] -> Set a
setDifferenceL s xs = S.difference s (S.fromList xs)

mapDifferenceL :: Ord k => Map k v -> [k] -> Map k v
mapDifferenceL m xs = M.difference m (M.fromList (map (\x -> (x, ())) xs))

mapDifferenceS :: Ord k => Map k v -> Set k -> Map k v
mapDifferenceS m s = M.difference m (M.fromSet (const ()) s)

unsafeLookup :: (Ord k, Show k, Show v) => String -> k -> Map k v -> v
unsafeLookup msg k kvs = fromMaybe (error msg') (M.lookup k kvs)
  where
    msg' = msg ++ ": " ++ show k ++ " in " ++ show (M.toList kvs)

(//) :: Ord k => Map k v -> Map k v -> Map k v
(//) = M.unionWith (\_ x -> x)
