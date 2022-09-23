{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( start
  ) where

import           Control.Monad                  ( unless )
import           SDL                            ( ($=)
                                                , ControllerButtonEventData
                                                  ( ControllerButtonEventData
                                                  )
                                                , Event(eventPayload)
                                                , EventPayload
                                                  ( ControllerButtonEvent
                                                  , JoyButtonEvent
                                                  , KeyboardEvent
                                                  )
                                                , InputMotion(Pressed)
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
                                                , clear
                                                , createRenderer
                                                , createWindow
                                                , defaultRenderer
                                                , defaultWindow
                                                , destroyWindow
                                                , initializeAll
                                                , pollEvents
                                                , present
                                                , rendererDrawColor
                                                )

start :: IO ()
start = do
  initializeAll
  window   <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
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
