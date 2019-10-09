module Data.Matrix.Storable where

    import GHC.TypeLits (KnownNat)
    import Data.IndexedListLiterals (IndexedListLiterals)
    import Foreign.Storable (Storable, peek, poke)
    import Foreign.Ptr (castPtr)
    import Graphics.Rendering.OpenGL.GL (GLmatrix)

    import qualified Data.Vector.Storable.Sized as Storable.Sized
    import qualified Data.Matrix.Generic as Generic
    import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL.CoordTrans
    import qualified Graphics.Rendering.OpenGL.GL as GL

    type Matrix = Generic.Matrix Storable.Sized.Vector

    fromTuple ∷ (IndexedListLiterals rows m cols, IndexedListLiterals cols n a, KnownNat m, KnownNat n, Storable a, Storable cols) ⇒ rows → Matrix m n a
    fromTuple rows = Generic.Matrix
        $! Storable.Sized.map Storable.Sized.fromTuple
            $ Storable.Sized.fromTuple rows

    toGLmatrix ∷ GL.MatrixComponent a ⇒ Matrix 4 4 a → IO (GLmatrix a)
    toGLmatrix p = GL.CoordTrans.withNewMatrix GL.CoordTrans.RowMajor
        $ \ptr → poke (castPtr ptr) p

    fromGLmatrix ∷ GL.MatrixComponent a ⇒ GLmatrix a → IO (Matrix 4 4 a)
    fromGLmatrix (p ∷ GLmatrix a) = GL.CoordTrans.withMatrix p $ \case
        GL.CoordTrans.RowMajor → peek . castPtr
        GL.CoordTrans.ColumnMajor → undefined



    {-

    mapM ∷ (KnownNat n, Storable a, Storable b, Monad t) ⇒ (a → t b) → Matrix m n a → t (Matrix m n b)
    mapM f (Generic.Matrix p) = Generic.Matrix <$> Storable.Sized.mapM (Storable.Sized.mapM f) p


   getMatrixComponents desiredOrder mat =
      withMatrix mat $ \order p ->
        if desiredOrder == order
           then peekArray 16 p
           else mapM (peekElemOff p) [ 0, 4,  8, 12,
                                       1, 5,  9, 13,
                                       2, 6, 10, 14,
                                       3, 7, 11, 15 ]

    -}
