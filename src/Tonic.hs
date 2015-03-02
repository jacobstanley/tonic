{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Tonic where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Tonic.Types
import           Tonic.Utils

------------------------------------------------------------------------

pattern F32     = Fmt F 32
pattern NF32    = NumTy (Fmt F 32)
pattern JString = ObjTy "java/lang/String"

foo0 :: Term String
foo0 =
    Let [c] (Copy [Num 42 F32]) $
    Let [x] (InvokeBinary (Add F32) (Var c) (Var c)) $
    Let [z] (InvokeBinary (Mul F32) (Var c) (Var c)) $
    Let [w] (InvokeBinary (Sub F32) (Var z) (Var x)) $
    Let [y] (InvokeStatic float2string [Var x]) $
    Let [out] (GetStatic sysOut) $
    Return (InvokeVirtual println (Var out) [Var y])
  where
    (c,x,y,z,w,out) = ("c","x","y","z","w","out")

foo1 :: Term String
foo1 =
    Let [x] (Copy [Num 9 F32]) $
    Let [y] (Copy [Num 1 F32]) $
    Let [w] (Copy [Num 42 F32]) $
    letrec [ (u, Const  NF32                   (Return (Copy [Num 42 F32])))
           , (t, Lambda (FunType [] [NF32]) [] (Return (Copy [Num 1 F32]))) ] $
    Let [z] (InvokeBinary (Add F32) (Var x) (Var y)) $
    Return (InvokeStatic float2string [Var z])
  where
    (x,y,z,w,u,t) = ("x","y","z","w","u","t")

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

float2string :: SMethod
float2string = SMethod "java/lang/Float" "toString" (MethodType [NF32] (Just JString))

sysOut :: SField
sysOut = SField  "java/lang/System" "out" (ObjTy "java/io/PrintStream")

println :: IMethod
println = IMethod "java/io/PrintStream" "println" (MethodType [ObjTy "java/lang/String"] Nothing)

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
    Copy              xs -> fvOfAtoms xs
    Invoke        _ f xs -> fvOfAtom f `S.union` fvOfAtoms xs
    InvokeUnary   _ x    -> fvOfAtom x
    InvokeBinary  _ x y  -> fvOfAtom x `S.union` fvOfAtom y
    InvokeVirtual _ i xs -> fvOfAtom i `S.union` fvOfAtoms xs
    InvokeSpecial _ i xs -> fvOfAtom i `S.union` fvOfAtoms xs
    InvokeStatic  _   xs -> fvOfAtoms xs
    GetField      _ i    -> fvOfAtom i
    PutField      _ i x  -> fvOfAtom i `S.union` fvOfAtom x
    GetStatic     _      -> S.empty
    PutStatic     _   x  -> fvOfAtom x

fvOfBinding :: Ord n => Binding n -> Set n
fvOfBinding binding = case binding of
    Lambda _ ns x -> fvOfTerm x `setDifferenceL` ns
    Const  _    x -> fvOfTerm x

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
    Var x     -> Var (unsafeLookup "renameAtom" x names)
    Num x fmt -> Num x fmt
    Str x     -> Str x

renameAtoms :: (Ord a, Show a, Show b) => Map a b -> [Atom a] -> [Atom b]
renameAtoms names = map (renameAtom names)

renameTail :: (Ord a, Show a, Show b) => Map a b -> Tail a -> Tail b
renameTail names tl = case tl of
    Copy              xs -> Copy                                 (renameAtoms names xs)
    Invoke        t f xs -> Invoke        t (renameAtom names f) (renameAtoms names xs)
    InvokeUnary   o x    -> InvokeUnary   o (renameAtom names x)
    InvokeBinary  o x y  -> InvokeBinary  o (renameAtom names x) (renameAtom names y)
    InvokeVirtual m i xs -> InvokeVirtual m (renameAtom names i) (renameAtoms names xs)
    InvokeSpecial m i xs -> InvokeSpecial m (renameAtom names i) (renameAtoms names xs)
    InvokeStatic  m   xs -> InvokeStatic  m                      (renameAtoms names xs)
    GetField      f i    -> GetField      f (renameAtom names i)
    PutField      f i x  -> PutField      f (renameAtom names i) (renameAtom names x)
    GetStatic     f      -> GetStatic     f
    PutStatic     f   x  -> PutStatic     f (renameAtom names x)

renameBindings :: (Ord a, Ord b, Show a, Show b) => [b] -> Map a b -> Bindings a -> ([b], Map a b, Bindings b)
renameBindings gen names as = (gen2, names', bs)
  where
    gen1       = drop (M.size as) gen
    names'     = names `mapUnionR` M.fromList (M.keys as `zip` gen)
    (gen2, bs) = renameBindingsList gen1 names' (M.toList as)

renameBindingsList :: (Ord a, Ord b, Show a, Show b) => [b] -> Map a b -> [(a, Binding a)] -> ([b], Bindings b)
renameBindingsList gen names bindings = case bindings of

    [(k, Lambda t ns x)] -> let (ns', gen1) = splitAt (length ns) gen
                                names'      = names `mapUnionR` M.fromList (ns `zip` gen)
                                (gen2, x')  = renameTerm gen1 names' x
                                k'          = unsafeLookup "renameBindingsList" k names
                            in
                                (gen2, M.singleton k' (Lambda t ns' x'))

    [(k, Const t x)]     -> let (gen', x') = renameTerm gen names x
                                k'         = unsafeLookup "renameBindingsList" k names
                            in
                                (gen', M.singleton k' (Const t x'))

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
                       names'      = names `mapUnionR` M.fromList (ns `zip` gen)
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
    Var x -> M.findWithDefault atom x subs
    _     -> atom

substAtoms :: Ord n => Map n (Atom n) -> [Atom n] -> [Atom n]
substAtoms subs = map (substAtom subs)

substTail :: Ord n => Map n (Atom n) -> Tail n -> Tail n
substTail subs tl = case tl of
    Copy              xs -> Copy                               (substAtoms subs xs)
    Invoke        t f xs -> Invoke        t (substAtom subs f) (substAtoms subs xs)
    InvokeUnary   o x    -> InvokeUnary   o (substAtom subs x)
    InvokeBinary  o x y  -> InvokeBinary  o (substAtom subs x) (substAtom subs y)
    InvokeVirtual m i xs -> InvokeVirtual m (substAtom subs i) (substAtoms subs xs)
    InvokeSpecial m i xs -> InvokeSpecial m (substAtom subs i) (substAtoms subs xs)
    InvokeStatic  m   xs -> InvokeStatic  m                    (substAtoms subs xs)
    GetField      f i    -> GetField      f (substAtom subs i)
    PutField      f i x  -> PutField      f (substAtom subs i) (substAtom subs x)
    GetStatic     f      -> GetStatic     f
    PutStatic     f   x  -> PutStatic     f (substAtom subs x)

substBinding :: Ord n => Map n (Atom n) -> Binding n -> Binding n
substBinding subs binding = case binding of
    Lambda t ns x -> Lambda t ns (substTerm (subs `mapDifferenceL` ns) x)
    Const  t    x -> Const  t    (substTerm subs x)

substTerm :: Ord n => Map n (Atom n) -> Term n -> Term n
substTerm subs term = case term of
    Return x      -> Return (substTail subs x)
    Iff    i t e  -> Iff (substAtom subs i) (substTerm subs t) (substTerm subs e)
    Let    ns x y -> Let ns (substTail subs x) (substTerm (subs `mapDifferenceL` ns) y)
    LetRec bs x   -> let subs' = subs `mapDifferenceS` bvOfBindings bs
                     in  LetRec (M.map (substBinding subs') bs) (substTerm subs' x)

------------------------------------------------------------------------
-- Simplifier

simplifyBinding :: Ord n => Binding n -> Binding n
simplifyBinding binding = case binding of
    Lambda t ns x -> Lambda t ns (simplifyTerm x)
    Const  t    x -> Const  t    (simplifyTerm x)

simplifyTerm :: Ord n => Term n -> Term n
simplifyTerm term = case term of
    Return x            -> Return x
    Iff (Num 0 _) _ e   -> simplifyTerm e
    Iff (Num _ _) t _   -> simplifyTerm t
    Iff i t e           -> Iff i (simplifyTerm t) (simplifyTerm e)
    Let ns (Copy ns') y -> simplifyTerm (substTerm (M.fromList (ns `zip` ns')) y)
    Let ns x          y -> Let ns x (simplifyTerm y)
    LetRec bs x         -> LetRec (M.map simplifyBinding bs) (simplifyTerm x)

------------------------------------------------------------------------
-- Dead Code Elimination

deadBinding :: Ord n => Binding n -> Binding n
deadBinding binding = case binding of
    Lambda t ns x -> Lambda t ns (deadTerm x)
    Const  t    x -> Const  t    (deadTerm x)

deadTerm :: Ord n => Term n -> Term n
deadTerm term = case term of
    Return x    -> Return x
    Iff i t e   -> Iff i (deadTerm t) (deadTerm e)

    Let ns x y  -> let y'  = deadTerm y
                       fvs = fvOfTerm y'
                   in
                       if S.null (setIntersectionL fvs ns)
                       then y'
                       else Let ns x y'

    LetRec bs x -> let x'  = deadTerm x
                       fvs = fvOfTerm x'
                       bs' = findRequired fvs (M.map deadBinding bs)
                   in
                       if M.null bs'
                       then x'
                       else LetRec bs' x'

findRequired :: Ord n => Set n -> Bindings n -> Bindings n
findRequired vs bs | vs == vs' = bs'
                   | otherwise = findRequired vs' bs
  where
    bs' = bs `mapIntersectionS` vs
    vs' = vs `S.union` fvOfBindings bs'

------------------------------------------------------------------------
-- Inliner

inlineBinding :: (Ord n, Show n) => Bindings n -> Binding n -> Binding n
inlineBinding env binding = case binding of
    Lambda t ns x -> Lambda t ns (inlineTerm (env `mapDifferenceL` ns) x)
    Const  t    x -> Const  t    (inlineTerm env x)

inlineTerm :: (Ord n, Show n) => Bindings n -> Term n -> Term n
inlineTerm env term = case term of
    Return (Invoke _ (Var f) xs) ->
      case unsafeLookup "inlineTerm" f env of
        Lambda _ ns tm -> inlineTerm env $ substTerm (M.fromList (ns `zip` xs)) tm
        _              -> error ("inlineTerm: can't inline " ++ show f ++ ", not in scope")

    Return x  -> Return x
    Iff i t e -> Iff i (inlineTerm env t) (inlineTerm env e)

    Let ns (Invoke _ (Var f) xs) y ->
      case unsafeLookup "inlineTerm" f env of
        Lambda _ ns' tm -> inlineTerm env $ letTerm ns (substTerm (M.fromList (ns' `zip` xs)) tm) y
        _               -> error ("inlineTerm: can't inline " ++ show f ++ ", not in scope")

    Let ns x y  -> Let ns x (inlineTerm env y)

    -- XXX check this knot tying is ok
    LetRec bs x -> let bs'  = M.map (inlineBinding env') bs
                       env' = env `mapUnionR` bs'
                       x'   = inlineTerm env' x
                   in
                       LetRec bs' x'

letTerm :: [n] -> Term n -> Term n -> Term n
letTerm ns term cont = case term of
    Return x    -> Let ns x cont
    Iff i t e   -> Iff i (letTerm ns t cont) (letTerm ns e cont)
    Let ns' x y -> Let ns' x (letTerm ns y cont)
    LetRec bs x -> LetRec bs (letTerm ns x cont)

