module Data.Transform () where

    import Data.Vector.Sized (Vector)
    import Data.Matrix (Matrix)

    data Signature n a where
        Identity ∷ Signature n a
        Translation ∷ Vector 3 n → Signature n a
        Rotation ∷ Double → Vector 3 Double → Signature n a
        Scale ∷ n → Vector 3 n → Signature n a
        Compose ∷ Signature n a → Signature n a → Signature n a
        Transform ∷ Matrix 4 4 n → Matrix 4 4 n → Signature n a
