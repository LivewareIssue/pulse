{-# LANGUAGE DerivingVia, UndecidableInstances #-}

module Data.Matrix.Generic (
        Matrix (..),
        convert,
        identity,
        transpose,
        scale,
        perspective,
        null,
        rotation,
        (!+!),
        (!-!),
        (!*!),
        (!*)
    ) where

    import Prelude hiding (null)

    import Control.Applicative (liftA2)
    import Data.Foldable (foldl')
    import Data.VectorSpace
    import GHC.TypeLits (Nat, natVal, KnownNat)
    import Data.Proxy (Proxy (..))
    import Data.Finite (Finite, getFinite)
    import Data.Ring (Ring (..))
    import Data.Functor.Rep (index, tabulate, Representable (..), distributeRep)
    import Data.Field (Field (..))
    import Data.Functor.Compose (Compose (..))
    import Data.Kind (Type, Constraint)
    import Data.Distributive (Distributive (..))
    import Data.Semigroup (Semigroup (..))
    import Data.Traversable (foldMapDefault)
    import Foreign.Storable

    import qualified Data.Vector as Unsized
    import qualified Data.Vector.Generic as Generic.Unsized
    import qualified Data.Vector.Generic.Sized as Generic.Sized
    import qualified Data.Vector.Sized as Sized
    import qualified Data.Finite as Finite

    newtype Matrix v (m ∷ Nat) (n ∷ Nat) a = Matrix (v m (v n a))
        deriving stock Functor
        deriving Representable via (Compose (v m) (v n))
        deriving Applicative   via (Compose (v m) (v n))

    deriving stock instance Eq (v m (v n a)) ⇒ Eq (Matrix v m n a)

    deriving via (∀ a (v ∷ Nat → Type → Type) (m ∷ Nat) (n ∷ Nat). v m (v n a))
        instance Storable (v m (v n a)) ⇒ Storable (Matrix v m n a)

    deriving via (∀ a (v ∷ Nat → Type → Type) (m ∷ Nat) (n ∷ Nat). v m (v n a))
        instance Show (v m (v n a)) ⇒ Show (Matrix v m n a)

    instance (Representable (v m), Representable (v n)) ⇒ Distributive (Matrix v m n) where
        distribute = distributeRep

    instance (Traversable (v m), Traversable (v n)) ⇒ Foldable (Matrix v m n) where
        foldMap = foldMapDefault

    instance (Traversable (v m), Traversable (v n)) ⇒ Traversable (Matrix v m n) where
        traverse f (Matrix p) = (Matrix . getCompose) <$> (traverse f (Compose p))

    -- where vectors are finite cartesian products over a ring
    type family CoordinateSpace (v ∷ Nat → Type → Type) (m ∷ Nat) a ∷ Constraint where
        CoordinateSpace v m a = (
                FiniteRep v m,
                Applicative (v m),
                Foldable (v m),
                Ring a,
                VectorSpace (v m a),
                Scalar (v m a) ~ a
            )

    type family FiniteRep (f ∷ Nat → Type → Type) (m ∷ Nat) ∷ Constraint where
        FiniteRep f m = (Representable (f m), Rep (f m) ~ Finite m)

    instance (CoordinateSpace v m a) ⇒ AdditiveGroup (Matrix v m m a) where
        zeroV = null
        (^+^) = (!+!)
        negateV p = ((negateV oneV) ^*^) <$> p

    instance (CoordinateSpace v m a) ⇒ Ring (Matrix v m m a) where
        oneV = identity
        (^*^) = (!*!)

    instance (CoordinateSpace v m a) ⇒ VectorSpace (Matrix v m m a) where
        type Scalar (Matrix v m m a) = a
        s *^ p = (s ^*^) <$> p

    instance (CoordinateSpace v m a) ⇒ Semigroup (Matrix v m m a) where
        (<>) = (^*^)

    instance (CoordinateSpace v m a) ⇒ Monoid (Matrix v m m a) where
        mempty = identity

    infixl 6 !+!
    (!+!) ∷ (CoordinateSpace v m a, AdditiveGroup (v n a)) ⇒ Matrix v m n a → Matrix v m n a → Matrix v m n a
    Matrix p !+! Matrix q = Matrix $! liftA2 (^+^) p q

    infixl 6 !-!
    (!-!) ∷ (CoordinateSpace v m a, AdditiveGroup (v n a)) ⇒ Matrix v m n a → Matrix v m n a → Matrix v m n a
    Matrix p !-! Matrix q = Matrix $! liftA2 (^-^) p q

    infixl 7 !*!
    (!*!) ∷ (CoordinateSpace v m a, CoordinateSpace v n a, CoordinateSpace v p a) ⇒ Matrix v m n a → Matrix v n p a → Matrix v m p a
    Matrix p !*! Matrix q = Matrix $! fmap (\r → foldl' (^+^) zeroV $ liftA2 (*^) r q) p

    infixl 7 !*
    (!*) ∷ (Applicative (v n), Foldable (v n), Functor (v m), InnerSpace a) ⇒ Matrix v m n a → v n a → v m (Scalar a)
    Matrix a !* b = fmap (\c → foldl' (^+^) zeroV $ liftA2 (<.>) c b) a

    convert ∷ (Functor v, Generic.Unsized.Vector v a, Generic.Unsized.Vector v (Generic.Sized.Vector w n a), Generic.Unsized.Vector w a, Generic.Unsized.Vector w (Generic.Sized.Vector w n a)) ⇒ Matrix (Generic.Sized.Vector v) m n a → Matrix (Generic.Sized.Vector w) m n a
    convert (Matrix p) = Matrix $! Generic.Sized.convert $ Generic.Sized.convert <$> p

    null ∷ CoordinateSpace v m a ⇒ Matrix v m m a
    null = tabulate $ const zeroV

    identity ∷ CoordinateSpace v m a ⇒ Matrix v m m a
    identity = tabulate $ \(i,j) → if (i `Finite.equals` j) then oneV else zeroV

    transpose ∷ (Distributive (v n), Functor (v m), KnownNat m, KnownNat n) ⇒ Matrix v m n a → Matrix v n m a
    transpose (Matrix p) = Matrix $! distribute p

    scale ∷ CoordinateSpace v i a ⇒ a → Matrix v i i a
    scale s = s *^ identity

    frustum ∷ (FiniteRep v 4, Field a) ⇒ a → a → a → a → a → a → Matrix v 4 4 a
    frustum left right top bottom near far = tabulate m
        where
            m(0,0) = (twoV ^*^ near) ^/^ (right ^-^ left)
            m(1,1) = (twoV ^*^ near) ^/^ (top ^-^ bottom)
            m(2,0) = (right ^+^ left) ^/^ (right ^-^ left)
            m(2,1) = (top ^+^ bottom) ^/^ (top ^-^ bottom)
            m(2,2) = (negateV (far ^+^ near)) ^/^ (far ^-^ near)
            m(2,3) = negateV oneV
            m(3,2) = (negateV (twoV ^*^ far ^*^ near)) ^/^ (far ^-^ near)
            m(_,_) = zeroV

            twoV = oneV ^+^ oneV

    rotation ∷ (FiniteRep v 4, FiniteRep v 3, InnerSpace (v 3 a), Scalar (v 3 a) ~ a, Ring a, Floating a) ⇒ a → v 3 a → Matrix v 4 4 a
    rotation θ axis = tabulate m
        where
            u = index (normalized axis) 0
            v = index (normalized axis) 1
            w = index (normalized axis) 2

            m(1,1) = cos θ + (u^2 * (1 - cos θ))
            m(1,2) = (u * v * (1 - cos θ)) - (w * sin θ)
            m(1,3) = (u * w * (1 - cos θ)) + (v * sin θ)
            m(2,1) = (v * u * (1 - cos θ)) + (w * sin θ)
            m(2,2) = cos θ + (v^2 * (1 - cos θ))
            m(2,3) = (v * w * (1 - cos θ)) - (u * sin θ)
            m(3,1) = (w * u * (1 - cos θ)) - (v * sin θ)
            m(3,2) = (w * v * (1 - cos θ)) + (u * sin θ)
            m(3,3) = cos θ + (w^2 * (1 - cos θ))
            m(4,4) = oneV
            m(_,_) = zeroV

    perspective ∷ (FiniteRep v 4, Field a, Floating a) ⇒ a → a → a → a → Matrix v 4 4 a
    perspective fovY aspect near far = frustum left right bottom top near far
        where
            top    = tan(fovY / 2) * near
            bottom = -top
            right  = top * aspect
            left   = -right
