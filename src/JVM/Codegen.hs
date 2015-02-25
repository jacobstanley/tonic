{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JVM.Codegen where

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

import           JVM.Bytecode

------------------------------------------------------------------------

data Class = Class
    { cAccess     :: [ClassAccess]
    , cName       :: ClassRef
    , cSuper      :: ClassRef
    , cInterfaces :: [ClassRef]
    , cFields     :: [Field]
    , cMethods    :: [Method]
    , cSourceFile :: Maybe Text
    } deriving (Eq, Ord, Show, Read)

data ClassAccess =
      C'Public     -- ^ Declared public; may be accessed from outside its package.
    | C'Final      -- ^ Declared final; no subclasses allowed.
    | C'Super      -- ^ Treat superclass methods specially when invoked by the invokespecial instruction.
    | C'Interface  -- ^ Is an interface, not a class.
    | C'Abstract   -- ^ Declared abstract; must not be instantiated.
    | C'Synthetic  -- ^ Declared synthetic; not present in the source code.
    | C'Annotation -- ^ Declared as an annotation type.
    | C'Enum       -- ^ Declared as an enum type.
    deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------

data Field = Field
    { fAccess :: [FieldAccess]
    , fName   :: Text
    , fType   :: Type
    } deriving (Eq, Ord, Show, Read)

data FieldAccess =
      F'Public    -- ^ Declared public; may be accessed from outside its package.
    | F'Private   -- ^ Declared private; usable only within the defining class.
    | F'Protected -- ^ Declared protected; may be accessed within subclasses.
    | F'Static    -- ^ Declared static.
    | F'Final     -- ^ Declared final; never directly assigned to after object construction (JLS §17.5).
    | F'Volatile  -- ^ Declared volatile; cannot be cached.
    | F'Transient -- ^ Declared transient; not written or read by a persistent object manager.
    | F'Synthetic -- ^ Declared synthetic; not present in the source code.
    | F'Enum      -- ^ Declared as an element of an enum.
    deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------

data Method = Method
    { mAccess :: [MethodAccess]
    , mName   :: Text
    , mType   :: Type
    , mCode   :: Maybe Code
    } deriving (Eq, Ord, Show, Read)

newtype Type = Type { unType :: Text }
    deriving (Eq, Ord, Show, Read)

data MethodAccess =
      M'Public       -- ^ Declared public; may be accessed from outside its package.
    | M'Private      -- ^ Declared private; accessible only within the defining class.
    | M'Protected    -- ^ Declared protected; may be accessed within subclasses.
    | M'Static       -- ^ Declared static.
    | M'Final        -- ^ Declared final; must not be overridden (§5.4.5).
    | M'Synchronized -- ^ Declared synchronized; invocation is wrapped by a monitor use.
    | M'Bridge       -- ^ A bridge method, generated by the compiler.
    | M'VarArgs      -- ^ Declared with variable number of arguments.
    | M'Native       -- ^ Declared native; implemented in a language other than Java.
    | M'Abstract     -- ^ Declared abstract; no implementation is provided.
    | M'Strict       -- ^ Declared strictfp; floating-point mode is FP-strict.
    | M'Synthetic    -- ^ Declared synthetic; not present in the source code.
    deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------

data Code = Code
    { cMaxStack     :: Word16 -- TODO calculate automatically
    , cMaxLocals    :: Word16 -- TODO calculate automatically
    , cInstructions :: [Instruction]
    } deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------

newtype ClassRef = ClassRef { unClassRef :: Text }
    deriving (Eq, Ord, Show, Read)

data FieldRef = FieldRef
    { fClass    :: ClassRef
    , fNameType :: NameType
    } deriving (Eq, Ord, Show, Read)

data MethodRef = MethodRef
    { mClass    :: ClassRef
    , mNameType :: NameType
    } deriving (Eq, Ord, Show, Read)

data NameType = NameType
    { nName :: Text
    , nType :: Type
    } deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------
-- Constant Pool

data ConstPool = ConstPool
    { cpNext      :: ConstRef
    , cpValues    :: [ConstVal]
    , cpUtf8s     :: Map Text      ConstRef
    , cpIntegers  :: Map Int32     ConstRef
    , cpFloats    :: Map Float     ConstRef
    , cpLongs     :: Map Int64     ConstRef
    , cpDoubles   :: Map Double    ConstRef
    , cpStrings   :: Map Text      ConstRef
    , cpClasses   :: Map ClassRef  ConstRef
    , cpFields    :: Map FieldRef  ConstRef
    , cpMethods   :: Map MethodRef ConstRef
    , cpNameTypes :: Map NameType  ConstRef
    } deriving (Eq, Ord, Show, Read)

newtype ConstRef = ConstRef { unConstRef :: Word16 }
    deriving (Eq, Ord, Show, Read, Enum)

data ConstVal =
      ConstUtf8     Text
    | ConstInteger  Int32
    | ConstFloat    Float
    | ConstLong     Int64
    | ConstDouble   Double
    | ConstString   ConstRef
    | ConstClass    ConstRef
    | ConstField    ConstRef ConstRef
    | ConstMethod   ConstRef ConstRef
    | ConstNameType ConstRef ConstRef
    deriving (Eq, Ord, Show, Read)

emptyConstPool :: ConstPool
emptyConstPool = ConstPool (ConstRef 1) []
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty

------------------------------------------------------------------------

type VarIndex = Word16

data Instruction =
      AConstNull

    | IConst Int32
    | LConst Int64
    | FConst Float
    | DConst Double
    | SConst Text

    | ILoad VarIndex
    | LLoad VarIndex
    | FLoad VarIndex
    | DLoad VarIndex
    | ALoad VarIndex

    | IStore VarIndex
    | LStore VarIndex
    | FStore VarIndex
    | DStore VarIndex
    | AStore VarIndex

    | IAdd
    | LAdd
    | FAdd
    | DAdd

    | GetStatic FieldRef
    | PutStatic FieldRef
    | GetField  FieldRef
    | PutField  FieldRef

    | InvokeVirtual MethodRef
    | InvokeStatic  MethodRef

    | IReturn
    | LReturn
    | FReturn
    | DReturn
    | AReturn
    | Return

    deriving (Eq, Ord, Show, Read)
------------------------------------------------------------------------

newtype CP a = CP { unCP :: State ConstPool a }
  deriving (Functor, Applicative, Monad, MonadState ConstPool)

runCP :: CP a -> (a, ConstPool)
runCP m = runState (unCP m) emptyConstPool

addConst :: (Ord k)
         => (k -> CP ConstVal)
         -> (ConstPool -> Map k ConstRef)
         -> (ConstPool -> Map k ConstRef -> ConstPool)
         -> k
         -> CP ConstRef
addConst mkVal getMap setMap constant = do
    m <- gets (M.lookup constant . getMap)
    case m of
      Just x  -> return x
      Nothing -> do
        val <- mkVal constant
        ref <- state (insertVal val)
        modify (\cp -> setMap cp (M.insert constant ref (getMap cp)))
        return ref

insertVal :: ConstVal -> ConstPool -> (ConstRef, ConstPool)
insertVal val cp@ConstPool{..} =
    (cpNext, cp { cpNext = next, cpValues = val : cpValues })
  where
    next = case val of
      ConstLong   _ -> succ (succ cpNext)
      ConstDouble _ -> succ (succ cpNext)
      _             -> succ cpNext

------------------------------------------------------------------------

addUtf8Ref :: Text -> CP ConstRef
addUtf8Ref = addConst mk cpUtf8s (\cp m -> cp { cpUtf8s = m })
  where
    mk = pure . ConstUtf8

addString :: Text -> CP ConstRef
addString = addConst mk cpStrings (\cp m -> cp { cpStrings = m })
  where
    mk x = ConstString <$> addUtf8Ref x

addInteger :: Int32 -> CP ConstRef
addInteger = addConst mk cpIntegers (\cp m -> cp { cpIntegers = m })
  where
    mk = pure . ConstInteger

addFloat :: Float -> CP ConstRef
addFloat = addConst mk cpFloats (\cp m -> cp { cpFloats = m })
  where
    mk = pure . ConstFloat

addLong :: Int64 -> CP ConstRef
addLong = addConst mk cpLongs (\cp m -> cp { cpLongs = m })
  where
    mk = pure . ConstLong

addDouble :: Double -> CP ConstRef
addDouble = addConst mk cpDoubles (\cp m -> cp { cpDoubles = m })
  where
    mk = pure . ConstDouble

addNameType :: NameType -> CP ConstRef
addNameType = addConst mk cpNameTypes (\cp m -> cp { cpNameTypes = m })
  where
    mk (NameType name typ) = ConstNameType <$> addUtf8Ref name <*> addUtf8Ref (unType typ)

addMethodRef :: MethodRef -> CP ConstRef
addMethodRef = addConst mk cpMethods (\cp m -> cp { cpMethods = m })
  where
    mk (MethodRef cls nt) = ConstMethod <$> addClassRef cls <*> addNameType nt

addFieldRef :: FieldRef -> CP ConstRef
addFieldRef = addConst mk cpFields (\cp m -> cp { cpFields = m })
  where
    mk (FieldRef cls nt) = ConstField <$> addClassRef cls <*> addNameType nt

addClassRef :: ClassRef -> CP ConstRef
addClassRef = addConst mk cpClasses (\cp m -> cp { cpClasses = m })
  where
    mk (ClassRef name) = ConstClass <$> addUtf8Ref name

------------------------------------------------------------------------

bClass :: Class -> Builder
bClass Class{..} = bClassHeader
                <> bConstPool cp
                <> b
  where
    (b, cp) = runCP $ do
        this  <- bClassRef cName
        super <- bClassRef cSuper
        is    <- bInterfaces cInterfaces
        fs    <- bFields cFields
        ms    <- bMethods cMethods
        attrs <- bAttributes [("SourceFile", bUtf8Ref <$> cSourceFile)]
        return $ bClassAccess cAccess
              <> this
              <> super
              <> is <> fs <> ms
              <> attrs

bClassHeader :: Builder
bClassHeader = word32BE magic <> word16BE minor <> word16BE major
  where
    magic = 0xCAFEBABE
    major = 51
    minor = 0

------------------------------------------------------------------------

bConstPool :: ConstPool -> Builder
bConstPool ConstPool{..} = word16BE cpCount
                        <> mconcat (map go (reverse cpValues))
  where
    cpCount = fromIntegral (length cpValues) + 1

    go (ConstUtf8 txt) = word8 1 <> word16BE n <> byteString bs
      where
        bs = T.encodeUtf8 txt
        n  = fromIntegral (B.length bs)

    go (ConstInteger x)   = word8 3 <> int32BE x
    go (ConstFloat   x)   = word8 4 <> floatBE x
    go (ConstLong    x)   = word8 5 <> int64BE x
    go (ConstDouble  x)   = word8 6 <> doubleBE x
    go (ConstString  ref) = word8 8 <> bConstRef ref

    go (ConstClass ref)          = word8 7  <> bConstRef ref
    go (ConstField cref nref)    = word8 9  <> bConstRef cref <> bConstRef nref
    go (ConstMethod cref nref)   = word8 10 <> bConstRef cref <> bConstRef nref
    go (ConstNameType nref tref) = word8 12 <> bConstRef nref <> bConstRef tref

bConstRef :: ConstRef -> Builder
bConstRef = word16BE . unConstRef

bUtf8Ref :: Text -> CP Builder
bUtf8Ref = (bConstRef <$>) . addUtf8Ref

bClassRef :: ClassRef -> CP Builder
bClassRef = (bConstRef <$>) . addClassRef

bFieldRef :: FieldRef -> CP Builder
bFieldRef = (bConstRef <$>) . addFieldRef

bMethodRef :: MethodRef -> CP Builder
bMethodRef = (bConstRef <$>) . addMethodRef

------------------------------------------------------------------------

bInterfaces :: [ClassRef] -> CP Builder
bInterfaces is = do
    refs <- mapM bClassRef is
    return $ word16BE n
          <> mconcat refs
  where
    n = fromIntegral (length is)

------------------------------------------------------------------------

bFields :: [Field] -> CP Builder
bFields fs = do
    bs <- mapM bField fs
    return $ word16BE (fromIntegral (length fs))
          <> mconcat bs

bField :: Field -> CP Builder
bField Field{..} = do
    name <- bUtf8Ref fName
    typ  <- bUtf8Ref (unType fType)
    return $ bFieldAccess fAccess
          <> name
          <> typ
          <> word16BE 0 -- attributes

------------------------------------------------------------------------

bMethods :: [Method] -> CP Builder
bMethods ms = do
    bs <- mapM bMethod ms
    return $ word16BE (fromIntegral (length ms))
          <> mconcat bs

bMethod :: Method -> CP Builder
bMethod Method{..} = do
    name  <- bUtf8Ref mName
    typ   <- bUtf8Ref (unType mType)
    attrs <- bAttributes [("Code", bCode <$> mCode)]
    return $ bMethodAccess mAccess
          <> name
          <> typ
          <> attrs

------------------------------------------------------------------------

bAttributes :: [(Text, Maybe (CP Builder))] -> CP Builder
bAttributes as = do
    bs <- mapM (uncurry bAttribute) as'
    return $ word16BE (fromIntegral (length as))
          <> mconcat bs
  where
    as' = mapMaybe go as

    go (_, Nothing) = Nothing
    go (t, Just b)  = Just (t, b)

bAttribute :: Text -> CP Builder -> CP Builder
bAttribute name b = do
    ref <- addUtf8Ref name
    lbs <- toLazyByteString <$> b
    return $ bConstRef ref
          <> word32BE (fromIntegral (L.length lbs))
          <> lazyByteString lbs

------------------------------------------------------------------------

bClassAccess :: [ClassAccess] -> Builder
bClassAccess = word16BE . foldl1' (.|.) . map go
  where
    go C'Public     = 0x1
    go C'Final      = 0x10
    go C'Super      = 0x20
    go C'Interface  = 0x200
    go C'Abstract   = 0x400
    go C'Synthetic  = 0x1000
    go C'Annotation = 0x2000
    go C'Enum       = 0x4000

bFieldAccess :: [FieldAccess] -> Builder
bFieldAccess = word16BE . foldl1' (.|.) . map go
  where
    go F'Public    = 0x1
    go F'Private   = 0x2
    go F'Protected = 0x4
    go F'Static    = 0x8
    go F'Final     = 0x10
    go F'Volatile  = 0x40
    go F'Transient = 0x80
    go F'Synthetic = 0x1000
    go F'Enum      = 0x4000

bMethodAccess :: [MethodAccess] -> Builder
bMethodAccess = word16BE . foldl1' (.|.) . map go
  where
    go M'Public       = 0x1
    go M'Private      = 0x2
    go M'Protected    = 0x4
    go M'Static       = 0x8
    go M'Final        = 0x10
    go M'Synchronized = 0x20
    go M'Bridge       = 0x40
    go M'VarArgs      = 0x80
    go M'Native       = 0x100
    go M'Abstract     = 0x400
    go M'Strict       = 0x800
    go M'Synthetic    = 0x1000

------------------------------------------------------------------------

bCode :: Code -> CP Builder
bCode Code{..} = do
    bcs <- mapM bytecodeOfInstruction cInstructions
    let lbs = toLazyByteString . mconcat . map bytecode $ bcs
    return $ word16BE cMaxStack
          <> word16BE cMaxLocals
          <> word32BE (fromIntegral (L.length lbs))
          <> lazyByteString lbs
          <> word16BE 0 -- exception table
          <> word16BE 0 -- attributes

------------------------------------------------------------------------

bytecodeOfInstruction :: Instruction -> CP Bytecode
bytecodeOfInstruction i = case i of

    -- Constants --

    AConstNull  -> pure B'AConst_null

    IConst (-1) -> pure B'IConst_m1
    IConst 0    -> pure B'IConst_0
    IConst 1    -> pure B'IConst_1
    IConst 2    -> pure B'IConst_2
    IConst 3    -> pure B'IConst_3
    IConst 4    -> pure B'IConst_4
    IConst 5    -> pure B'IConst_5

    IConst x | isByte  x -> pure (B'BIPush (fromIntegral x))
             | isShort x -> pure (B'SIPush (fromIntegral x))
             | otherwise -> ldc <$> addInteger x

    LConst 0 -> pure B'LConst_0
    LConst 1 -> pure B'LConst_1
    LConst x -> ldc <$> addLong x

    FConst 0 -> pure B'FConst_0
    FConst 1 -> pure B'FConst_1
    FConst 2 -> pure B'FConst_2
    FConst x -> ldc <$> addFloat x

    DConst 0 -> pure B'DConst_0
    DConst 1 -> pure B'DConst_1
    DConst x -> ldc <$> addDouble x

    SConst x -> ldc <$> addString x


    -- Loads --

    ILoad 0 -> pure B'ILoad_0
    ILoad 1 -> pure B'ILoad_1
    ILoad 2 -> pure B'ILoad_2
    ILoad 3 -> pure B'ILoad_3

    ILoad x | isWide x  -> pure (B'ILoad (fromIntegral x))
            | otherwise -> pure (B'ILoad_W x)

    LLoad 0 -> pure B'LLoad_0
    LLoad 1 -> pure B'LLoad_1
    LLoad 2 -> pure B'LLoad_2
    LLoad 3 -> pure B'LLoad_3

    LLoad x | isWide x  -> pure (B'LLoad (fromIntegral x))
            | otherwise -> pure (B'LLoad_W x)

    FLoad 0 -> pure B'FLoad_0
    FLoad 1 -> pure B'FLoad_1
    FLoad 2 -> pure B'FLoad_2
    FLoad 3 -> pure B'FLoad_3

    FLoad x | isWide x  -> pure (B'FLoad (fromIntegral x))
            | otherwise -> pure (B'FLoad_W x)

    DLoad 0 -> pure B'DLoad_0
    DLoad 1 -> pure B'DLoad_1
    DLoad 2 -> pure B'DLoad_2
    DLoad 3 -> pure B'DLoad_3

    DLoad x | isWide x  -> pure (B'DLoad (fromIntegral x))
            | otherwise -> pure (B'DLoad_W x)

    ALoad 0 -> pure B'ALoad_0
    ALoad 1 -> pure B'ALoad_1
    ALoad 2 -> pure B'ALoad_2
    ALoad 3 -> pure B'ALoad_3

    ALoad x | isWide x  -> pure (B'ALoad (fromIntegral x))
            | otherwise -> pure (B'ALoad_W x)


    -- Stores --

    IStore 0 -> pure B'IStore_0
    IStore 1 -> pure B'IStore_1
    IStore 2 -> pure B'IStore_2
    IStore 3 -> pure B'IStore_3

    IStore x | isWide x  -> pure (B'IStore (fromIntegral x))
             | otherwise -> pure (B'IStore_W x)

    LStore 0 -> pure B'LStore_0
    LStore 1 -> pure B'LStore_1
    LStore 2 -> pure B'LStore_2
    LStore 3 -> pure B'LStore_3

    LStore x | isWide x  -> pure (B'LStore (fromIntegral x))
             | otherwise -> pure (B'LStore_W x)

    FStore 0 -> pure B'FStore_0
    FStore 1 -> pure B'FStore_1
    FStore 2 -> pure B'FStore_2
    FStore 3 -> pure B'FStore_3

    FStore x | isWide x  -> pure (B'FStore (fromIntegral x))
             | otherwise -> pure (B'FStore_W x)

    DStore 0 -> pure B'DStore_0
    DStore 1 -> pure B'DStore_1
    DStore 2 -> pure B'DStore_2
    DStore 3 -> pure B'DStore_3

    DStore x | isWide x  -> pure (B'DStore (fromIntegral x))
             | otherwise -> pure (B'DStore_W x)

    AStore 0 -> pure B'AStore_0
    AStore 1 -> pure B'AStore_1
    AStore 2 -> pure B'AStore_2
    AStore 3 -> pure B'AStore_3

    AStore x | isWide x  -> pure (B'AStore (fromIntegral x))
             | otherwise -> pure (B'AStore_W x)


    -- Arithmetic --

    IAdd -> pure B'IAdd
    LAdd -> pure B'LAdd
    FAdd -> pure B'FAdd
    DAdd -> pure B'DAdd

    -- Misc --

    IReturn -> pure B'IReturn
    LReturn -> pure B'LReturn
    FReturn -> pure B'FReturn
    DReturn -> pure B'DReturn
    AReturn -> pure B'AReturn
    Return -> pure B'Return

    GetField  x -> B'GetField  . unConstRef <$> addFieldRef x
    PutField  x -> B'PutField  . unConstRef <$> addFieldRef x
    GetStatic x -> B'GetStatic . unConstRef <$> addFieldRef x
    PutStatic x -> B'PutStatic . unConstRef <$> addFieldRef x

    InvokeVirtual x -> B'InvokeVirtual . unConstRef <$> addMethodRef x
    InvokeStatic  x -> B'InvokeStatic  . unConstRef <$> addMethodRef x

  where
    ldc (ConstRef ref) | isWide ref = B'LdC (fromIntegral ref)
                       | otherwise  = B'LdC_W ref

    isWide  x = x <= maxW8
    isByte  x = x >= minI8  && x <= maxI8
    isShort x = x >= minI16 && x <= maxI16

    maxW8 = fromIntegral (maxBound :: Word8)

    minI8 = fromIntegral (minBound :: Int8)
    maxI8 = fromIntegral (maxBound :: Int8)

    minI16 = fromIntegral (minBound :: Int16)
    maxI16 = fromIntegral (maxBound :: Int16)