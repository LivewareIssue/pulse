{-# LANGUAGE ViewPatterns #-}

module Spec where

    import Test.QuickCheck (quickCheck, Property)
    import Test.QuickCheck.Monadic (monadicIO, run)
    import Test.QuickCheck.Instances.Vector ()
    import Data.Ring
    import Data.Finite (Finite, getFinite)
    import Data.Functor.Rep (Representable (..))
    import Data.VectorSpace
    import Data.Matrix (Matrix)
    import GHC.TypeLits (KnownNat)
    import Foreign.Storable (Storable)

    import qualified Data.Matrix as Matrix
    import qualified Data.Matrix.Generic as Generic
    import qualified Data.Matrix.Storable as Storable
    import qualified Data.Vector.Storable.Sized as Storable.Sized
    import qualified Data.Vector.Sized as Sized

    import qualified Graphics.Rendering.OpenGL.GL as GL

    import Orphans ()

    main ∷ IO ()
    main = do
        putStrLn "Matrix and Storable.Matrix are isomorphic"
        quickCheck $ prop_isomorphism (Matrix.toStorable @4 @4 @Double) Matrix.fromStorable
        putStrLn mempty
        putStrLn "Storable.Matrix and GLmatrix are isomorphic"
        quickCheck $ prop_isomorphismIO (Storable.toGLmatrix . (Matrix.toStorable @4 @4 @Double)) ((fmap Matrix.fromStorable) . Storable.fromGLmatrix)
        putStrLn mempty
        putStrLn "Matrices have ring structure, assuming"
        putStrLn " • 3⨉3 matrices have ring structure"
        putStrLn " • Integer has ring structure"
        putStrLn mempty
        putStrLn "Matrix multiplication is associative"
        quickCheck $ prop_ringMultiplicationAssociative @(Matrix 4 4 Integer)
        putStrLn mempty
        putStrLn "The identity matrix is the identity w.r.t matrix multiplication"
        quickCheck $ prop_ringMultiplicativeIdentity @(Matrix 4 4 Integer)
        putStrLn mempty
        putStrLn "Matrix multiplication distributes over matrix addition"
        quickCheck $ prop_ringDistributive @(Matrix 4 4 Integer)
        putStrLn mempty
        putStrLn "Rationals have field structure"
        putStrLn mempty
        putStrLn "Rational multiplication is commutative"
        quickCheck $ prop_fieldMultiplicationCommutative @Rational

    prop_isomorphismIO ∷ Eq a ⇒ (a → IO b) → (b → IO a) → a → Property
    prop_isomorphismIO f g x = monadicIO $ do
        y ← run $! (f x) >>= g
        return $! x == y

    prop_isomorphism ∷ Eq a ⇒ (a → b) → (b → a) → a → Bool
    prop_isomorphism f g x = x == g(f(x))

    prop_ringMultiplicationAssociative ∷ (Eq v, Ring v) ⇒ v → v → v → Bool
    prop_ringMultiplicationAssociative a b c = a ^*^ (b ^*^ c) == (a ^*^ b) ^*^ c

    prop_ringMultiplicativeIdentity ∷ (Eq v, Ring v) ⇒ v → Bool
    prop_ringMultiplicativeIdentity v = (oneV ^*^ v == v) && (v ^*^ oneV == v)

    prop_ringDistributive ∷ (Eq v, Ring v) ⇒ v → v → v → Bool
    prop_ringDistributive a b c = leftDistributive && rightDistributive
        where
            leftDistributive  = a ^*^ (b ^+^ c) == (a ^*^ b) ^+^ (a ^*^ c)
            rightDistributive = (a ^+^ b) ^*^ c == (a ^*^ c) ^+^ (b ^*^ c)

    prop_fieldMultiplicationCommutative ∷ (Eq v, Ring v) ⇒ v → v → Bool
    prop_fieldMultiplicationCommutative a b = (a ^*^ b) == (b ^*^ a)
