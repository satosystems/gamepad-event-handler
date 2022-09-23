{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( start
  ) where

import           Control.Monad                  ( unless )
import           Data.Foldable                  ( forM_ )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vector                   as V
import           SDL                            ( ($=)
                                                , ControllerButtonEventData
                                                  ( ControllerButtonEventData
                                                  )
                                                , Event(eventPayload)
                                                , EventPayload
                                                  ( ControllerButtonEvent
                                                  , JoyAxisEvent
                                                  , JoyButtonEvent
                                                  , KeyboardEvent
                                                  )
                                                , InputMotion(Pressed)
                                                , JoyAxisEventData
                                                  ( JoyAxisEventData
                                                  )
                                                , JoyButtonEventData
                                                  ( JoyButtonEventData
                                                  )
                                                , KeyboardEventData
                                                  ( KeyboardEventData
                                                  , keyboardEventKeyMotion
                                                  , keyboardEventKeysym
                                                  )
                                                , pattern KeycodeQ
                                                , Keysym(Keysym, keysymKeycode)
                                                , Renderer
                                                , V4(V4)
                                                , availableJoysticks
                                                , clear
                                                , closeJoystick
                                                , createRenderer
                                                , createWindow
                                                , defaultRenderer
                                                , defaultWindow
                                                , destroyWindow
                                                , initializeAll
                                                , numJoysticks
                                                , openJoystick
                                                , pollEvents
                                                , present
                                                , rendererDrawColor
                                                )

start :: IO ()
start = do
  initializeAll
  putStrLn "Press Q when quit"
  joysticks <- numJoysticks
  putStr "numJoysticks: "
  print joysticks
  availables <- availableJoysticks
  putStr "availableJoysticks: "
  print availables
  window    <- createWindow "My SDL Application" defaultWindow
  renderer  <- createRenderer window (-1) defaultRenderer
  mJoystick <- if not . null $ availables
    then openJoystick (V.head availables) Data.Functor.<&> Just
    else return Nothing
  appLoop renderer
  forM_ mJoystick closeJoystick
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          keyboardEventKeyMotion keyboardEvent
            == Pressed
            && keysymKeycode (keyboardEventKeysym keyboardEvent)
            == KeycodeQ
        _ -> False
  let qPressed = any eventIsQPress events
  mapM_
    (\event -> case eventPayload event of
      ControllerButtonEvent (ControllerButtonEventData _ button state) -> do
        putStrLn "ControllerButtonEvent"
        print button
        print state
      JoyButtonEvent (JoyButtonEventData _ button state) -> do
        putStrLn "JoyButtonEvent"
        print button
        print state
      JoyAxisEvent (JoyAxisEventData _ axis value) -> do
        putStrLn "JoyAxisEvent"
        print axis
        print value
      KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ keycode _)) -> do
        putStrLn "KeyboardEvent"
        print keycode
      _ -> return ()
    )
    events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed $ appLoop renderer
