module Data.Matrix where

    import GHC.TypeLits (KnownNat)
    import Data.IndexedListLiterals (IndexedListLiterals)
    import Foreign.Storable (Storable)
    import Graphics.Rendering.OpenGL.GL (GLmatrix)

    import qualified Data.Vector.Sized as Sized
    import qualified Data.Vector.Generic.Sized as Generic.Sized
    import qualified Data.Vector.Storable.Sized as Storable.Sized

    import qualified Data.Matrix.Generic as Generic
    import qualified Data.Matrix.Storable as Storable

    import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL.CoordTrans
    import qualified Graphics.Rendering.OpenGL.GL as GL

    type Matrix = Generic.Matrix Sized.Vector

    fromTuple ∷ (IndexedListLiterals rows m cols, IndexedListLiterals cols n a, KnownNat m, KnownNat n) ⇒ rows → Matrix m n a
    fromTuple rows = Generic.Matrix
        $! Sized.fromTuple <$> Sized.fromTuple rows

    toStorable ∷ ∀ m n a. (KnownNat n, Storable a) ⇒ Matrix m n a → Storable.Matrix m n a
    toStorable = Generic.convert

    fromStorable ∷ ∀ m n a. (KnownNat n, Storable a) ⇒ Storable.Matrix m n a → Matrix m n a
    fromStorable (Generic.Matrix p) = Generic.Matrix $! Generic.Sized.convert $ Storable.Sized.map Generic.Sized.convert p

    toGLmatrix ∷ GL.MatrixComponent a ⇒ Matrix 4 4 a → IO (GLmatrix a)
    toGLmatrix = Storable.toGLmatrix . toStorable

    fromGLmatix ∷ GL.MatrixComponent a ⇒ GLmatrix a → IO (Matrix 4 4 a)
    fromGLmatix = (fmap fromStorable) . Storable.fromGLmatrix
