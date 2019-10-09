{-# LANGUAGE OverloadedStrings, DerivingVia #-}

module Pulse where

    --import Control.Exception.Safe (Exception, throw, bracket, try)
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad.Managed.Safe (runManaged)
    import Data.Traversable (for)
    import Data.Text (Text)
    import SDL (($=))

    import qualified Graphics.Rendering.OpenGL.GL as GL
    import qualified Pulse.Video.UI as UI
    import qualified Pulse.Video.Shaders as Shader

    import qualified Data.Matrix as Matrix
    import qualified Data.Matrix.Storable as Storable

    main ∷ IO ()
    main = runManaged $ do

        -- Initialize the SDL library with every subsystem flag
        UI.initializeSubsystems

        -- Create a window with the specified title and flags
        window ← UI.window windowTitle UI.defaultWindowConfig

        -- Request a rendering-context with the profile specified by the window
        context ← UI.renderingContext window

        -- Compile the specified shaders
        shaders ← for shaderSources Shader.compile

        -- Create an empty program-object and attach each shader
        program ← Shader.program shaders

        {- Perform pre-link setup; create a vertex-array object and a vertex-
           buffer object, enable generic vertex attributes, and set the active
           texture unit -}
        Shader.prelinkSetup (GL.AttribLocation <$> [1,2,3]) (GL.TextureUnit 0) program

        -- Link the program-object
        liftIO $ Shader.link program

        -- Install the linked program-object as part of the current rendering state
        GL.currentProgram $= Just program

        where
            windowTitle ∷ Text
            windowTitle = "title"

            shaderSources ∷ [Shader.Source]
            shaderSources = [
                    Shader.Source {
                        shaderType = GL.VertexShader,
                        sourcePath = "data/shaders/default.vert"
                    },
                    Shader.Source {
                        shaderType = GL.FragmentShader,
                        sourcePath = "data/shaders/default.frag"
                    }
                ]
