module Pulse.Object where

    import Control.Exception (bracket)
    import qualified Graphics.Rendering.OpenGL.GL as GL

    withObject ∷ GL.GeneratableObjectName a ⇒ (a → IO b) → IO b
    withObject = bracket GL.genObjectName GL.deleteObjectName
