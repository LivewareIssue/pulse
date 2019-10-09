module Pulse.Video.UI (
    Managed,
    initializeSubsystems,
    SDL.Window,
    SDL.WindowConfig,
    window,
    defaultWindowConfig,
    SDL.GLContext,
    renderingContext

) where

    import Control.Exception (bracket, bracket_)
    import Control.Monad.Managed.Safe (Managed, managed, managed_)
    import Data.Text (Text)
    import qualified SDL

    initializeSubsystems ∷ Managed ()
    initializeSubsystems = managed_ $ bracket_ SDL.initializeAll SDL.quit

    window ∷ Text → SDL.WindowConfig → Managed SDL.Window
    window title config = managed $ bracket aquire release
        where
            aquire  = SDL.createWindow title config
            release = SDL.destroyWindow

    defaultWindowConfig ∷ SDL.WindowConfig
    defaultWindowConfig = SDL.defaultWindow {
        SDL.windowOpenGL = Just $ SDL.defaultOpenGL {
            SDL.glProfile = SDL.Core SDL.Normal 3 2
            --SDL.windowResizable = True
        }
    }

    renderingContext ∷ SDL.Window → Managed SDL.GLContext
    renderingContext window = managed $ bracket aquire release
        where
            aquire  = SDL.glCreateContext window
            release = SDL.glDeleteContext
