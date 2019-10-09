module Pulse.Scene.Camera where

    import Data.Vector.Sized (Vector)
    import Data.Matrix.Storable.Sized (Matrix)

    data Camera n = Camera { position ∷ Vector 3 n, target ∷ Vector 3 n }
        deriving (Eq, Show)

    lookAt ∷ Camrea n → Transform n
    lookAt = undefined
