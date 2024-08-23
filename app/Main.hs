{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

--import Graphics.Gloss
--import Graphics.Gloss.Interface.IO.Game (Event)
import qualified SDL
--import qualified SDL.Init as SDL  -- qualified import for initialization functions
--import qualified SDL.Raw as Raw
--import qualified SDL.Raw.Event as Event -- for isGameController
--import qualified SDL.Input.Joystick as Joystick
import qualified SDL.Input.GameController as GameController

import Control.Monad
--import Foreign.C.Types (CInt)
--import Data.Word (Word32)


main :: IO()
main = do
  initializeSDL
  mainloop
  SDL.quit 
  putStrLn "SDL cleaned up and exited"


mainloop :: IO ()
mainloop = do
  quit <- handleEvents
  unless quit mainloop

initializeSDL :: IO()
initializeSDL = do
  SDL.initialize [SDL.InitGameController, SDL.InitJoystick]
  -- -let mappingsPath = "data/gamecontrollerdb.txt"
  -- GameController.addControllerMappingsFromFile mappingsPath 
  putStrLn "Game cCntroller subsystem initialized"

  controllers <- GameController.availableControllers 
  if null controllers
    then putStrLn "No game controllers found"
    else do
      putStrLn $ "Number of game controllers detected: " ++ show (length controllers)
      forM_ controllers $ \controllerDevice -> do 
        _ <- GameController.openController controllerDevice 
        putStrLn $ "Opened game controller: " ++ show (GameController.gameControllerDeviceName controllerDevice)

handleEvents :: IO Bool
handleEvents = do
    events <- SDL.pollEvents
    let controllerEvents = filter (isControllerEvent . SDL.eventPayload) events
    mapM_ (handleControllerEvent . SDL.eventPayload) controllerEvents

    return $ any (isQuitEvent . SDL.eventPayload) events

isControllerEvent :: SDL.EventPayload -> Bool
isControllerEvent event = do 
  case event of
    SDL.ControllerButtonEvent _ -> True
    SDL.ControllerAxisEvent _ -> True
    _ -> False

handleControllerEvent :: SDL.EventPayload -> IO ()
handleControllerEvent event = do
  putStrLn $ "Event detected: " ++ show event -- print all events for debugging
  case event of
    SDL.ControllerButtonEvent buttonEvent -> 
      putStrLn $ "Button " ++ show (SDL.controllerButtonEventButton buttonEvent) ++ " pressed"
    SDL.ControllerAxisEvent axisEvent -> 
      putStrLn $ "Axis " ++ show (SDL.controllerAxisEventAxis axisEvent) ++ " moved"
    _ -> return ()

isQuitEvent :: SDL.EventPayload -> Bool 
isQuitEvent event = case event of 
  SDL.QuitEvent -> True 
  _ -> False 


{-}
initializeSDL = do
    SDL.initialize [SDL.InitGameController]
    numControllers <- length GameController.availableControllers 
    mapM_ (GameController.open . fromIntegral) [0 .. numControllers - 1]  -- Open each controller
    return ()

initializeSDL = do
    SDL.initialize [SDL.InitGameController]
    -- availableControllers <- SDL.availableJoysticks
    availableControllers <- GameController.availableControllers
    -- mapM_ SDL.openGameController availableControllers
    mapM_ GameController.open availableControllers
    return ()
-}

{-}
-- Capture SDL2 events and translate them into a game state update
handleSDLEvents :: IO (Float, Float) -- Example: returning updated player position
handleSDLEvents = do
    events <- SDL.pollEvents
    let controllers = map SDL.eventPayload events
    let positionDelta = foldr (handleControllerEvent) (0, 0) controllers
    return positionDelta

handleControllerEvent :: SDL.EventPayload -> (Float, Float) -> (Float, Float)
handleControllerEvent (SDL.ControllerAxisEvent axisEvent) (dx, dy) =
    let axis = SDL.controllerAxisEventAxis axisEvent
        value = SDL.controllerAxisEventValue axisEvent
        magnitude = fromIntegral value / 32768.0
    in case axis of
         SDL.ControllerAxisLeftX -> (dx + magnitude, dy)
         SDL.ControllerAxisLeftY -> (dx, dy + magnitude)
         _                       -> (dx, dy)
handleControllerEvent _ pos = pos

-}

{-}
-- Main function to run the Gloss application
main :: IO ()
main = do
    initializeSDL
    playIO
        (InWindow "Gloss + SDL2 Input Example" (800, 600) (100, 100))
        black
        60
        (0, 0)  -- Initial position
        renderScene
        handleGlossEvent
        updateScene

-- Render the current scene
renderScene :: (Float, Float) -> IO Picture
renderScene (x, y) = return $ translate x y $ color red $ circleSolid 20

-- Handle Gloss events (no-op if you're handling everything via SDL)
handleGlossEvent :: Event -> (Float, Float) -> IO (Float, Float)
handleGlossEvent _ pos = return pos

-- Update the scene with input from SDL2
updateScene :: Float -> (Float, Float) -> IO (Float, Float)
updateScene _ (x, y) = do
    (dx, dy) <- handleSDLEvents
    return (x + dx, y + dy)
-}