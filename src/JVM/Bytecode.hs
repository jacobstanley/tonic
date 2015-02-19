module JVM.Bytecode
    ( Bytecode(..)
    , bytecode
    ) where

import Data.ByteString.Builder
import Data.Int (Int8, Int16, Int32)
import Data.Monoid ((<>))
import Data.Word (Word8, Word16)

------------------------------------------------------------------------

data Bytecode =
      B'NOp                   -- ^ 0x00 - perform no operation

    | B'AConst_null           -- ^ 0x01 - push a null reference onto the stack
    | B'IConst_m1             -- ^ 0x02 - load the int value -1 onto the stack
    | B'IConst_0              -- ^ 0x03 - load the int value 0 onto the stack
    | B'IConst_1              -- ^ 0x04 - load the int value 1 onto the stack
    | B'IConst_2              -- ^ 0x05 - load the int value 2 onto the stack
    | B'IConst_3              -- ^ 0x06 - load the int value 3 onto the stack
    | B'IConst_4              -- ^ 0x07 - load the int value 4 onto the stack
    | B'IConst_5              -- ^ 0x08 - load the int value 5 onto the stack
    | B'LConst_0              -- ^ 0x09 - push the long 0 onto the stack
    | B'LConst_1              -- ^ 0x0a - push the long 1 onto the stack
    | B'FConst_0              -- ^ 0x0b - push 0.0f on the stack
    | B'FConst_1              -- ^ 0x0c - push 1.0f on the stack
    | B'FConst_2              -- ^ 0x0d - push 2.0f on the stack
    | B'DConst_0              -- ^ 0x0e - push the constant 0.0 onto the stack
    | B'DConst_1              -- ^ 0x0f - push the constant 1.0 onto the stack

    | B'BIPush Int8           -- ^ 0x10 - push a byte onto the stack as an integer value
    | B'SIPush Int16          -- ^ 0x11 - push a short onto the stack

    | B'LdC    Word8          -- ^ 0x12 - push a constant #index from a constant pool (String, int or float) onto the stack
    | B'LdC_W  Word16         -- ^ 0x13 - push a constant #index from a constant pool (String, int or float) onto the stack
                              --            (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    | B'LdC2_W Word16         -- ^ 0x14 - push a constant #index from a constant pool (double or long) onto the stack
                              --            (wide index is constructed as indexbyte1 << 8 + indexbyte2)

    | B'ILoad Word8           -- ^ 0x15 - load an int value from a local variable #index
    | B'LLoad Word8           -- ^ 0x16 - load a long value from a local variable #index
    | B'FLoad Word8           -- ^ 0x17 - load a float value from a local variable #index
    | B'DLoad Word8           -- ^ 0x18 - load a double value from a local variable #index
    | B'ALoad Word8           -- ^ 0x19 - load a reference onto the stack from a local variable #index

    | B'ILoad_0               -- ^ 0x1a - load an int value from local variable 0
    | B'ILoad_1               -- ^ 0x1b - load an int value from local variable 1
    | B'ILoad_2               -- ^ 0x1c - load an int value from local variable 2
    | B'ILoad_3               -- ^ 0x1d - load an int value from local variable 3

    | B'LLoad_0               -- ^ 0x1e - load a long value from a local variable 0
    | B'LLoad_1               -- ^ 0x1f - load a long value from a local variable 1
    | B'LLoad_2               -- ^ 0x20 - load a long value from a local variable 2
    | B'LLoad_3               -- ^ 0x21 - load a long value from a local variable 3

    | B'FLoad_0               -- ^ 0x22 - load a float value from local variable 0
    | B'FLoad_1               -- ^ 0x23 - load a float value from local variable 1
    | B'FLoad_2               -- ^ 0x24 - load a float value from local variable 2
    | B'FLoad_3               -- ^ 0x25 - load a float value from local variable 3

    | B'DLoad_0               -- ^ 0x26 - load a double from local variable 0
    | B'DLoad_1               -- ^ 0x27 - load a double from local variable 1
    | B'DLoad_2               -- ^ 0x28 - load a double from local variable 2
    | B'DLoad_3               -- ^ 0x29 - load a double from local variable 3

    | B'ALoad_0               -- ^ 0x2a - load a reference onto the stack from local variable 0
    | B'ALoad_1               -- ^ 0x2b - load a reference onto the stack from local variable 1
    | B'ALoad_2               -- ^ 0x2c - load a reference onto the stack from local variable 2
    | B'ALoad_3               -- ^ 0x2d - load a reference onto the stack from local variable 3

    | B'IALoad                -- ^ 0x2e - load an int from an array
    | B'LALoad                -- ^ 0x2f - load a long from an array
    | B'FALoad                -- ^ 0x30 - load a float from an array
    | B'DALoad                -- ^ 0x31 - load a double from an array
    | B'AALoad                -- ^ 0x32 - load onto the stack a reference from an array
    | B'BALoad                -- ^ 0x33 - load a byte or Boolean value from an array
    | B'CALoad                -- ^ 0x34 - load a char from an array
    | B'SALoad                -- ^ 0x35 - load a short from an array

    | B'IStore Word8          -- ^ 0x36 - store int value into variable #index
    | B'LStore Word8          -- ^ 0x37 - store a long value in a local variable #index
    | B'FStore Word8          -- ^ 0x38 - store a float value into a local variable #index
    | B'DStore Word8          -- ^ 0x39 - store a double value into a local variable #index
    | B'AStore Word8          -- ^ 0x3a - store a reference into a local variable #index

    | B'IStore_0              -- ^ 0x3b - store int value into variable 0
    | B'IStore_1              -- ^ 0x3c - store int value into variable 1
    | B'IStore_2              -- ^ 0x3d - store int value into variable 2
    | B'IStore_3              -- ^ 0x3e - store int value into variable 3

    | B'LStore_0              -- ^ 0x3f - store a long value in a local variable 0
    | B'LStore_1              -- ^ 0x40 - store a long value in a local variable 1
    | B'LStore_2              -- ^ 0x41 - store a long value in a local variable 2
    | B'LStore_3              -- ^ 0x42 - store a long value in a local variable 3

    | B'FStore_0              -- ^ 0x43 - store a float value into local variable 0
    | B'FStore_1              -- ^ 0x44 - store a float value into local variable 1
    | B'FStore_2              -- ^ 0x45 - store a float value into local variable 2
    | B'FStore_3              -- ^ 0x46 - store a float value into local variable 3

    | B'DStore_0              -- ^ 0x47 - store a double into local variable 0
    | B'DStore_1              -- ^ 0x48 - store a double into local variable 1
    | B'DStore_2              -- ^ 0x49 - store a double into local variable 2
    | B'DStore_3              -- ^ 0x4a - store a double into local variable 3

    | B'AStore_0              -- ^ 0x4b - store a reference into local variable 0
    | B'AStore_1              -- ^ 0x4c - store a reference into local variable 1
    | B'AStore_2              -- ^ 0x4d - store a reference into local variable 2
    | B'AStore_3              -- ^ 0x4e - store a reference into local variable 3

    | B'IAStore               -- ^ 0x4f - store an int in an array
    | B'LAStore               -- ^ 0x50 - store a long to an array
    | B'FAStore               -- ^ 0x51 - store a float in an array
    | B'DAStore               -- ^ 0x52 - store a double in an array
    | B'AAStore               -- ^ 0x53 - store a reference in an array
    | B'BAStore               -- ^ 0x54 - store a byte or Boolean value in an array
    | B'CAStore               -- ^ 0x55 - store a char intan array
    | B'SAStore               -- ^ 0x56 - store a short in an array

    | B'Pop                   -- ^ 0x57 - discard the top value on the stack
    | B'Pop2                  -- ^ 0x58 - discard the top two values on the stack
                              --            (or one value, if it is a double or long)

    | B'Dup                   -- ^ 0x59 - duplicate the value on top of the stack
    | B'Dup_x1                -- ^ 0x5a - insert a copy of the top value into the stack two values from the top.
                              --            (value1 and value2 must not be of the type double or long)
    | B'Dup_x2                -- ^ 0x5b - insert a copy of the top value into the stack two
                              --            (if value2 is double or long it takes up the entry of value3, too)
                              --          or three values
                              --            (if value2 is neither double nor long)
                              --          from the top

    | B'Dup2                  -- ^ 0x5c - duplicate top two stack words
                              --            (two values, if value1 is not double nor long;
                              --             a single value, if value1 is double or long)
    | B'Dup2_x1               -- ^ 0x5d - duplicate two words and insert beneath third word (see explanation above)
    | B'Dup2_x2               -- ^ 0x5e - duplicate two words and insert beneath fourth word

    | B'Swap                  -- ^ 0x5f - swaps two top words on the stack
                              --            (note that value1 and value2 must not be double or long)

    | B'IAdd                  -- ^ 0x60 - add two ints
    | B'LAdd                  -- ^ 0x61 - add two longs
    | B'FAdd                  -- ^ 0x62 - add two floats
    | B'DAdd                  -- ^ 0x63 - add two doubles

    | B'ISub                  -- ^ 0x64 - subtract two ints
    | B'LSub                  -- ^ 0x65 - subtract two longs
    | B'FSub                  -- ^ 0x66 - subtract two floats
    | B'DSub                  -- ^ 0x67 - subtract two doubles

    | B'IMul                  -- ^ 0x68 - multiply two integers
    | B'LMul                  -- ^ 0x69 - multiply two longs
    | B'FMul                  -- ^ 0x6a - multiply two floats
    | B'DMul                  -- ^ 0x6b - multiply two doubles

    | B'IDiv                  -- ^ 0x6c - divide two integers
    | B'LDiv                  -- ^ 0x6d - divide two longs
    | B'FDiv                  -- ^ 0x6e - divide two floats
    | B'DDiv                  -- ^ 0x6f - divide two doubles

    | B'IRem                  -- ^ 0x70 - remainder of division of two ints
    | B'LRem                  -- ^ 0x71 - remainder of division of two longs
    | B'FRem                  -- ^ 0x72 - remainder of division of two floats
    | B'DRem                  -- ^ 0x73 - remainder of division of two doubles

    | B'INeg                  -- ^ 0x74 - negate an int
    | B'LNeg                  -- ^ 0x75 - negate a long
    | B'FNeg                  -- ^ 0x76 - negate a float
    | B'DNeg                  -- ^ 0x77 - negate a double

    | B'IShl                  -- ^ 0x78 - int shift left
    | B'LShl                  -- ^ 0x79 - bitwise shift left of a long value1 by int value2 positions

    | B'IShr                  -- ^ 0x7a - int arithmetic shift right
    | B'LShr                  -- ^ 0x7b - bitwise shift right of a long value1 by int value2 positions

    | B'IUShr                 -- ^ 0x7c - int logical shift right
    | B'LUShr                 -- ^ 0x7d - bitwise shift right of a long value1 by int value2 positions, unsigned

    | B'IAnd                  -- ^ 0x7e - bitwise AND of two ints
    | B'LAnd                  -- ^ 0x7f - bitwise AND of two longs
    | B'IOr                   -- ^ 0x80 - bitwise OR of two ints
    | B'LOr                   -- ^ 0x81 - bitwise OR of two longs
    | B'IXor                  -- ^ 0x82 - bitwise XOR of two ints
    | B'LXor                  -- ^ 0x83 - bitwise XOR of two longs

    | B'IInc Word8 Int8       -- ^ 0x84 - increment local variable #index by signed byte #const

    | B'I2L                   -- ^ 0x85 - convert an int into a long
    | B'I2F                   -- ^ 0x86 - convert an int into a float
    | B'I2D                   -- ^ 0x87 - convert an int into a double

    | B'L2I                   -- ^ 0x88 - convert a long to a int
    | B'L2F                   -- ^ 0x89 - convert a long to a float
    | B'L2D                   -- ^ 0x8a - convert a long to a double

    | B'F2I                   -- ^ 0x8b - convert a float to an int
    | B'F2L                   -- ^ 0x8c - convert a float to a long
    | B'F2D                   -- ^ 0x8d - convert a float to a double

    | B'D2I                   -- ^ 0x8e - convert a double to an int
    | B'D2L                   -- ^ 0x8f - convert a double to a long
    | B'D2F                   -- ^ 0x90 - convert a double to a float

    | B'I2B                   -- ^ 0x91 - convert an int into a byte
    | B'I2C                   -- ^ 0x92 - convert an int into a character
    | B'I2S                   -- ^ 0x93 - convert an int into a short

    | B'LCmp                  -- ^ 0x94 - compare two longs values
    | B'FCmpL                 -- ^ 0x95 - compare two floats
    | B'FCmpG                 -- ^ 0x96 - compare two floats
    | B'DCmpL                 -- ^ 0x97 - compare two doubles
    | B'DCmpG                 -- ^ 0x98 - compare two doubles

    | B'IfEq Int16            -- ^ 0x99 - if value is 0, branch to instruction at #branchoffset
    | B'IfNe Int16            -- ^ 0x9a - if value is not 0, branch to instruction at #branchoffset
    | B'IfLt Int16            -- ^ 0x9b - if value is less than 0, branch to instruction at #branchoffset
    | B'IfGe Int16            -- ^ 0x9c - if value is greater than or equal to 0, branch to instruction at #branchoffset
    | B'IfGt Int16            -- ^ 0x9d - if value is greater than 0, branch to instruction at #branchoffset
    | B'IfLe Int16            -- ^ 0x9e - if value is less than or equal to 0, branch to instruction at #branchoffset

    | B'If_ICmpEq Int16       -- ^ 0x9f - if ints are equal, branch to instruction at #branchoffset
    | B'If_ICmpNe Int16       -- ^ 0xa0 - if ints are not equal, branch to instruction at #branchoffset
    | B'If_ICmpLt Int16       -- ^ 0xa1 - if value1 is less than value2, branch to instruction at #branchoffset
    | B'If_ICmpGe Int16       -- ^ 0xa2 - if value1 is greater than or equal to value2, branch to instruction at #branchoffset
    | B'If_ICmpGt Int16       -- ^ 0xa3 - if value1 is greater than value2, branch to instruction at #branchoffset
    | B'If_ICmpLe Int16       -- ^ 0xa4 - if value1 is less than or equal to value2, branch to instruction at #branchoffset

    | B'If_ACmpEq Int16       -- ^ 0xa5 - if references are equal, branch to instruction at #branchoffset
    | B'If_ACmpNe Int16       -- ^ 0xa6 - if references are not equal, branch to instruction at #branchoffset

    | B'Goto Int16            -- ^ 0xa7 - goes to another instruction at #branchoffset
    | B'JSr  Int16            -- ^ 0xa8 - jump to subroutine at #branchoffset and place the return address on the stack
    | B'Ret  Word8            -- ^ 0xa9 - continue execution from address taken from a local variable #index
                              --            (the asymmetry with jsr is intentional)

    | B'TableSwitch           -- ^ 0xaa - continue execution from an address in the table at offset index
    | B'LookupSwitch          -- ^ 0xab - a target address is looked up from a table using a key and execution
                              --            continues from the instruction at that address

    | B'IReturn               -- ^ 0xac - return an int
    | B'LReturn               -- ^ 0xad - return a long
    | B'FReturn               -- ^ 0xae - return a float
    | B'DReturn               -- ^ 0xaf - return a double
    | B'AReturn               -- ^ 0xb0 - return a reference
    | B'Return                -- ^ 0xb1 - return void

    | B'GetStatic Word16      -- ^ 0xb2 - get static field, where field identified by #index in constant pool
    | B'PutStatic Word16      -- ^ 0xb3 - set static field, where field identified by #index in constant pool

    | B'GetField  Word16      -- ^ 0xb4 - get field, where field identified by #index in constant pool
    | B'PutField  Word16      -- ^ 0xb5 - set field, where field identified by #index in constant pool

    | B'InvokeVirtual	Word16        -- ^ 0xb6 - invoke virtual method identified by #index in constant pool
    | B'InvokeSpecial	Word16        -- ^ 0xb7 - invoke instance method identified by #index in constant pool
    | B'InvokeStatic    Word16        -- ^ 0xb8 - invoke static method identified by #index in constant pool
    | B'InvokeInterface Word16 Word8  -- ^ 0xb9 - invoke interface method identified by #index in constant pool
    | B'InvokeDynamic   Word16        -- ^ 0xba - invoke dynamic method identified by #index in constant pool

    | B'New       Word16      -- ^ 0xbb - create new object of type identified by #index in constant pool
    | B'NewArray  Word8       -- ^ 0xbc - create new array with count elements of primitive type identified by #atype
    | B'ANewArray Word16      -- ^ 0xbd - create new array with count elements of reference type identified by #index in constant pool

    | B'ArrayLength           -- ^ 0xbe - get the length of an array

    | B'AThrow                -- ^ 0xbf - throws an error or exception (the stack is cleared, leaving only the Throwable)

    | B'CheckCast  Word16     -- ^ 0xc0 - casts an object is to a certain type, identified by #index in constant pool
    | B'InstanceOf Word16     -- ^ 0xc1 - determines if an object is of a certain type, identified by #index in constant pool

    | B'MonitorEnter          -- ^ 0xc2 - enter monitor for object ("grab the lock" - start of synchronized() section)
    | B'MonitorExit           -- ^ 0xc3 - exit monitor for object ("release the lock" - end of synchronized() section)

    | B'ILoad_W Word16        -- ^ 0xc4 0x15 - load an int value from a local variable #index
    | B'LLoad_W Word16        -- ^ 0xc4 0x16 - load a long value from a local variable #index
    | B'FLoad_W Word16        -- ^ 0xc4 0x17 - load a float value from a local variable #index
    | B'DLoad_W Word16        -- ^ 0xc4 0x18 - load a double value from a local variable #index
    | B'ALoad_W Word16        -- ^ 0xc4 0x19 - load a reference onto the stack from a local variable #index

    | B'IStore_W Word16       -- ^ 0xc4 0x36 - store int value into variable #index
    | B'LStore_W Word16       -- ^ 0xc4 0x37 - store a long value in a local variable #index
    | B'FStore_W Word16       -- ^ 0xc4 0x38 - store a float value into a local variable #index
    | B'DStore_W Word16       -- ^ 0xc4 0x39 - store a double value into a local variable #index
    | B'AStore_W Word16       -- ^ 0xc4 0x3a - store a reference into a local variable #index

    | B'IInc_W Word16 Int16   -- ^ 0xc4 0x84 - increment local variable #index by signed byte #const

    | B'Ret_W Word16          -- ^ 0xc4 0xa9 - continue execution from address taken from a local variable #index
                              --                 (the asymmetry with jsr_w is intentional)

    | B'MultiANewArray Word16 Word8 -- ^ 0xc5 - create a new array of #dimensions with elements of type #index in constant pool

    | B'IfNull    Int16       -- ^ 0xc6 - if value is null, branch to instruction at #branchoffset
    | B'IfNonNull Int16       -- ^ 0xc7 - if value is not null, branch to instruction at #branchoffset

    | B'Goto_W Int32          -- ^ 0xc8 - goes to another instruction at #branchoffset
    | B'JSr_W  Int32          -- ^ 0xc9 - jump to subroutine at #branchoffset

    deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------

bytecode :: Bytecode -> Builder
bytecode bc = case bc of
    B'NOp                 -> word8 0x00
    B'AConst_null         -> word8 0x01
    B'IConst_m1           -> word8 0x02
    B'IConst_0            -> word8 0x03
    B'IConst_1            -> word8 0x04
    B'IConst_2            -> word8 0x05
    B'IConst_3            -> word8 0x06
    B'IConst_4            -> word8 0x07
    B'IConst_5            -> word8 0x08
    B'LConst_0            -> word8 0x09
    B'LConst_1            -> word8 0x0a
    B'FConst_0            -> word8 0x0b
    B'FConst_1            -> word8 0x0c
    B'FConst_2            -> word8 0x0d
    B'DConst_0            -> word8 0x0e
    B'DConst_1            -> word8 0x0f
    B'BIPush x            -> word8 0x10 <> int8 x
    B'SIPush x            -> word8 0x11 <> int16BE x
    B'LdC x               -> word8 0x12 <> word8 x
    B'LdC_W x             -> word8 0x13 <> word16BE x
    B'LdC2_W x            -> word8 0x14 <> word16BE x
    B'ILoad x             -> word8 0x15 <> word8 x
    B'LLoad x             -> word8 0x16 <> word8 x
    B'FLoad x             -> word8 0x17 <> word8 x
    B'DLoad x             -> word8 0x18 <> word8 x
    B'ALoad x             -> word8 0x19 <> word8 x
    B'ILoad_0             -> word8 0x1a
    B'ILoad_1             -> word8 0x1b
    B'ILoad_2             -> word8 0x1c
    B'ILoad_3             -> word8 0x1d
    B'LLoad_0             -> word8 0x1e
    B'LLoad_1             -> word8 0x1f
    B'LLoad_2             -> word8 0x20
    B'LLoad_3             -> word8 0x21
    B'FLoad_0             -> word8 0x22
    B'FLoad_1             -> word8 0x23
    B'FLoad_2             -> word8 0x24
    B'FLoad_3             -> word8 0x25
    B'DLoad_0             -> word8 0x26
    B'DLoad_1             -> word8 0x27
    B'DLoad_2             -> word8 0x28
    B'DLoad_3             -> word8 0x29
    B'ALoad_0             -> word8 0x2a
    B'ALoad_1             -> word8 0x2b
    B'ALoad_2             -> word8 0x2c
    B'ALoad_3             -> word8 0x2d
    B'IALoad              -> word8 0x2e
    B'LALoad              -> word8 0x2f
    B'FALoad              -> word8 0x30
    B'DALoad              -> word8 0x31
    B'AALoad              -> word8 0x32
    B'BALoad              -> word8 0x33
    B'CALoad              -> word8 0x34
    B'SALoad              -> word8 0x35
    B'IStore x            -> word8 0x36 <> word8 x
    B'LStore x            -> word8 0x37 <> word8 x
    B'FStore x            -> word8 0x38 <> word8 x
    B'DStore x            -> word8 0x39 <> word8 x
    B'AStore x            -> word8 0x3a <> word8 x
    B'IStore_0            -> word8 0x3b
    B'IStore_1            -> word8 0x3c
    B'IStore_2            -> word8 0x3d
    B'IStore_3            -> word8 0x3e
    B'LStore_0            -> word8 0x3f
    B'LStore_1            -> word8 0x40
    B'LStore_2            -> word8 0x41
    B'LStore_3            -> word8 0x42
    B'FStore_0            -> word8 0x43
    B'FStore_1            -> word8 0x44
    B'FStore_2            -> word8 0x45
    B'FStore_3            -> word8 0x46
    B'DStore_0            -> word8 0x47
    B'DStore_1            -> word8 0x48
    B'DStore_2            -> word8 0x49
    B'DStore_3            -> word8 0x4a
    B'AStore_0            -> word8 0x4b
    B'AStore_1            -> word8 0x4c
    B'AStore_2            -> word8 0x4d
    B'AStore_3            -> word8 0x4e
    B'IAStore             -> word8 0x4f
    B'LAStore             -> word8 0x50
    B'FAStore             -> word8 0x51
    B'DAStore             -> word8 0x52
    B'AAStore             -> word8 0x53
    B'BAStore             -> word8 0x54
    B'CAStore             -> word8 0x55
    B'SAStore             -> word8 0x56
    B'Pop                 -> word8 0x57
    B'Pop2                -> word8 0x58
    B'Dup                 -> word8 0x59
    B'Dup_x1              -> word8 0x5a
    B'Dup_x2              -> word8 0x5b
    B'Dup2                -> word8 0x5c
    B'Dup2_x1             -> word8 0x5d
    B'Dup2_x2             -> word8 0x5e
    B'Swap                -> word8 0x5f
    B'IAdd                -> word8 0x60
    B'LAdd                -> word8 0x61
    B'FAdd                -> word8 0x62
    B'DAdd                -> word8 0x63
    B'ISub                -> word8 0x64
    B'LSub                -> word8 0x65
    B'FSub                -> word8 0x66
    B'DSub                -> word8 0x67
    B'IMul                -> word8 0x68
    B'LMul                -> word8 0x69
    B'FMul                -> word8 0x6a
    B'DMul                -> word8 0x6b
    B'IDiv                -> word8 0x6c
    B'LDiv                -> word8 0x6d
    B'FDiv                -> word8 0x6e
    B'DDiv                -> word8 0x6f
    B'IRem                -> word8 0x70
    B'LRem                -> word8 0x71
    B'FRem                -> word8 0x72
    B'DRem                -> word8 0x73
    B'INeg                -> word8 0x74
    B'LNeg                -> word8 0x75
    B'FNeg                -> word8 0x76
    B'DNeg                -> word8 0x77
    B'IShl                -> word8 0x78
    B'LShl                -> word8 0x79
    B'IShr                -> word8 0x7a
    B'LShr                -> word8 0x7b
    B'IUShr               -> word8 0x7c
    B'LUShr               -> word8 0x7d
    B'IAnd                -> word8 0x7e
    B'LAnd                -> word8 0x7f
    B'IOr                 -> word8 0x80
    B'LOr                 -> word8 0x81
    B'IXor                -> word8 0x82
    B'LXor                -> word8 0x83
    B'IInc v i            -> word8 0x84 <> word8 v <> int8 i
    B'I2L                 -> word8 0x85
    B'I2F                 -> word8 0x86
    B'I2D                 -> word8 0x87
    B'L2I                 -> word8 0x88
    B'L2F                 -> word8 0x89
    B'L2D                 -> word8 0x8a
    B'F2I                 -> word8 0x8b
    B'F2L                 -> word8 0x8c
    B'F2D                 -> word8 0x8d
    B'D2I                 -> word8 0x8e
    B'D2L                 -> word8 0x8f
    B'D2F                 -> word8 0x90
    B'I2B                 -> word8 0x91
    B'I2C                 -> word8 0x92
    B'I2S                 -> word8 0x93
    B'LCmp                -> word8 0x94
    B'FCmpL               -> word8 0x95
    B'FCmpG               -> word8 0x96
    B'DCmpL               -> word8 0x97
    B'DCmpG               -> word8 0x98
    B'IfEq x              -> word8 0x99 <> int16BE x
    B'IfNe x              -> word8 0x9a <> int16BE x
    B'IfLt x              -> word8 0x9b <> int16BE x
    B'IfGe x              -> word8 0x9c <> int16BE x
    B'IfGt x              -> word8 0x9d <> int16BE x
    B'IfLe x              -> word8 0x9e <> int16BE x
    B'If_ICmpEq x         -> word8 0x9f <> int16BE x
    B'If_ICmpNe x         -> word8 0xa0 <> int16BE x
    B'If_ICmpLt x         -> word8 0xa1 <> int16BE x
    B'If_ICmpGe x         -> word8 0xa2 <> int16BE x
    B'If_ICmpGt x         -> word8 0xa3 <> int16BE x
    B'If_ICmpLe x         -> word8 0xa4 <> int16BE x
    B'If_ACmpEq x         -> word8 0xa5 <> int16BE x
    B'If_ACmpNe x         -> word8 0xa6 <> int16BE x
    B'Goto x              -> word8 0xa7 <> int16BE x
    B'JSr  x              -> word8 0xa8 <> int16BE x
    B'Ret  x              -> word8 0xa9 <> word8 x
    B'TableSwitch         -> error "JVM.Bytecode.bytecode: not implemented 0xaa"
    B'LookupSwitch        -> error "JVM.Bytecode.bytecode: not implemented 0xab"
    B'IReturn             -> word8 0xac
    B'LReturn             -> word8 0xad
    B'FReturn             -> word8 0xae
    B'DReturn             -> word8 0xaf
    B'AReturn             -> word8 0xb0
    B'Return              -> word8 0xb1
    B'GetStatic x         -> word8 0xb2 <> word16BE x
    B'PutStatic x         -> word8 0xb3 <> word16BE x
    B'GetField  x         -> word8 0xb4 <> word16BE x
    B'PutField  x         -> word8 0xb5 <> word16BE x
    B'InvokeVirtual   x   -> word8 0xb6 <> word16BE x
    B'InvokeSpecial   x   -> word8 0xb7 <> word16BE x
    B'InvokeStatic    x   -> word8 0xb8 <> word16BE x
    B'InvokeInterface x y -> word8 0xb9 <> word16BE x <> word8 y
    B'InvokeDynamic   x   -> word8 0xba <> word16BE x
    B'New       x         -> word8 0xbb <> word16BE x
    B'NewArray  t         -> word8 0xbc <> word8 t
    B'ANewArray x         -> word8 0xbd <> word16BE x
    B'ArrayLength         -> word8 0xbe
    B'AThrow              -> word8 0xbf
    B'CheckCast  x        -> word8 0xc0 <> word16BE x
    B'InstanceOf x        -> word8 0xc0 <> word16BE x
    B'MonitorEnter        -> word8 0xc2
    B'MonitorExit         -> word8 0xc3
    B'ILoad_W x           -> word8 0xc4 <> word8 0x15 <> word16BE x
    B'LLoad_W x           -> word8 0xc4 <> word8 0x16 <> word16BE x
    B'FLoad_W x           -> word8 0xc4 <> word8 0x17 <> word16BE x
    B'DLoad_W x           -> word8 0xc4 <> word8 0x18 <> word16BE x
    B'ALoad_W x           -> word8 0xc4 <> word8 0x19 <> word16BE x
    B'IStore_W x          -> word8 0xc4 <> word8 0x36 <> word16BE x
    B'LStore_W x          -> word8 0xc4 <> word8 0x37 <> word16BE x
    B'FStore_W x          -> word8 0xc4 <> word8 0x38 <> word16BE x
    B'DStore_W x          -> word8 0xc4 <> word8 0x39 <> word16BE x
    B'AStore_W x          -> word8 0xc4 <> word8 0x3a <> word16BE x
    B'IInc_W v i          -> word8 0xc4 <> word8 0x84 <> word16BE v <> int16BE i
    B'Ret_W x             -> word8 0xc4 <> word8 0xa9 <> word16BE x
    B'MultiANewArray d t  -> word8 0xc5 <> word16BE d <> word8 t
    B'IfNull    x         -> word8 0xc6 <> int16BE x
    B'IfNonNull x         -> word8 0xc7 <> int16BE x
    B'Goto_W x            -> word8 0xc8 <> int32BE x
    B'JSr_W  x            -> word8 0xc9 <> int32BE x
