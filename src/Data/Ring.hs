{-# LANGUAGE CPP #-}

module Data.Ring (Ring (..), AdditiveGroup (..)) where

    import Data.VectorSpace
    import Foreign.C.Types

    class AdditiveGroup v ⇒ Ring v where
        oneV  ∷ v
        (^*^) ∷ v → v → v

    instance AdditiveGroup Bool where
        zeroV   = False
        a ^+^ b = (a && not b) || (not a && b)
        negateV = not

    instance Ring Bool where
        oneV  = True
        (^*^) = (&&)

    #define DeriveViaNum(n) \
        instance Ring (n) where { oneV  = 1; (^*^) = (*) }

    DeriveViaNum(Rational)
    DeriveViaNum(Int)
    DeriveViaNum(Integer)
    DeriveViaNum(Float)
    DeriveViaNum(Double)
    DeriveViaNum(CSChar)
    DeriveViaNum(CInt)
    DeriveViaNum(CShort)
    DeriveViaNum(CLong)
    DeriveViaNum(CLLong)
    DeriveViaNum(CIntMax)
    DeriveViaNum(CDouble)
    DeriveViaNum(CFloat)
