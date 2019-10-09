{-# LANGUAGE UndecidableInstances #-}

module Orphans where

    import Control.Applicative (liftA2)
    import Data.VectorSpace
    import Data.Functor.Rep
    import GHC.TypeLits (KnownNat)
    import Data.Ring
    import Test.QuickCheck (Arbitrary (..))
    import Data.Matrix (Matrix)

    import qualified Data.Vector.Generic as Generic.Unsized
    import qualified Data.Vector.Sized as Sized
    import qualified Data.Vector.Generic.Sized as Generic.Sized
    import qualified Data.Vector.Storable.Sized as Storable.Sized
    import qualified Data.Matrix as Matrix

    instance (KnownNat m, Applicative (Generic.Sized.Vector v m), VectorSpace a, Generic.Unsized.Vector v a) ⇒ AdditiveGroup (Generic.Sized.Vector v m a) where
        zeroV = Generic.Sized.replicate zeroV
        (^+^) = liftA2 (^+^)
        negateV = fmap negateV

    instance (KnownNat m, Applicative (Generic.Sized.Vector v m), VectorSpace a, Generic.Unsized.Vector v a) ⇒ VectorSpace (Generic.Sized.Vector v m a) where
        type Scalar (Generic.Sized.Vector v m a) = Scalar a
        s *^ p = (s *^) <$> p

    instance (KnownNat m, Foldable v, Applicative (Generic.Sized.Vector v m), InnerSpace a, Generic.Unsized.Vector v a) ⇒ InnerSpace (Generic.Sized.Vector v m a) where
        p <.> q = foldl (^+^) zeroV $ liftA2 (<.>) p q

    instance Arbitrary a ⇒ Arbitrary (Matrix 4 4 a) where
        arbitrary = Matrix.fromTuple <$> arbitrary @((a,a,a,a), (a,a,a,a), (a,a,a,a), (a,a,a,a))
        shrink xs = sequenceA $ shrink <$> xs
