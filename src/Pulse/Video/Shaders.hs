module Pulse.Video.Shaders where

    import Control.Exception.Safe (bracket, bracket_, throw)
    import Control.Monad.Managed.Safe (managed, managed_, Managed)
    import Control.Monad (when, unless)
    import qualified Data.ByteString.Char8 as ByteString
    import Data.Foldable (traverse_)
    import Graphics.GL.Types (GLuint)
    import qualified Graphics.Rendering.OpenGL.GL as GL
    import Graphics.Rendering.OpenGL.GL (get, ($=))
    import Pulse.Object (withObject)
    import System.Exit (exitFailure)
    import Text.Printf (printf)

    program ∷ Traversable f ⇒ f GL.Shader → Managed GL.Program
    program shaders = managed $ bracket aquire release
        where
            aquire = do
                program ← GL.createProgram
                traverse_ (GL.attachShader program) shaders
                return program

            release program = do
                current ← get GL.currentProgram
                when (maybe False (== program) current) (GL.currentProgram $= Nothing)
                traverse_ (GL.detachShader program) shaders
                GL.deleteObjectName program

    prelinkSetup ∷ Traversable f ⇒ f GL.AttribLocation → GL.TextureUnit → GL.Program → Managed ()
    prelinkSetup attributeIndices textureUnit program = do
        (vao ∷ GL.VertexArrayObject) ← managed $ withObject
        GL.bindVertexArrayObject $= Just vao

        (vbo ∷ GL.BufferObject) ← managed $ withObject
        GL.bindBuffer GL.ArrayBuffer $= Just vbo

        traverse_ (\n → GL.vertexAttribArray n $= GL.Enabled) attributeIndices
        GL.activeTexture $= textureUnit

    data Source = Source { shaderType ∷ GL.ShaderType, sourcePath ∷ FilePath }

    compile ∷ Source → Managed GL.Shader
    compile (Source shaderType sourcePath) = managed $ bracket aquire GL.deleteObjectName
        where
            aquire = do
                shader ← GL.createShader shaderType
                source ← ByteString.readFile sourcePath

                GL.shaderSourceBS shader $= source
                GL.compileShader shader

                ok ← get $ GL.compileStatus shader
                unless ok $ do
                    GL.deleteObjectName shader
                    exitFailure

                return shader

    link ∷ GL.Program → IO ()
    link program = do
        GL.linkProgram program
        linked ← get $ GL.linkStatus program

        GL.validateProgram program
        valid ← get $ GL.validateStatus program

        unless (linked && valid) exitFailure
