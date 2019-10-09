{-# LANGUAGE CPP #-}

module Data.Field where

    import Prelude hiding (recip)
    import qualified Prelude

    import Data.VectorSpace
    import Data.Ring
    import Foreign.C.Types

    class Ring v ⇒ Field v where
        recip ∷ v → v
        (^/^) ∷ v → v → v

        default recip ∷ Ring v ⇒ v → v
        recip a = oneV ^/^ a

        default (^/^) ∷ Ring v ⇒ v → v → v
        a ^/^ b = a ^*^ (recip b)

    #define DeriveViaFractional(n) \
        instance Field (n) where { recip = Prelude.recip }

    DeriveViaFractional(Rational)
    DeriveViaFractional(Float)
    DeriveViaFractional(Double)
    DeriveViaFractional(CDouble)
    DeriveViaFractional(CFloat)
