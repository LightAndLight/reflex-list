{-# language TypeFamilies, FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
module Main where

import Reflex
import Reflex.List
import Reflex.Host.Basic

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = do
  var :: TVar String <- newTVarIO ""
  basicHostForever $ app var
  where
    app :: BasicGuestConstraints t m => TVar String -> BasicGuest t m ()
    app var = do
      (eInput, inputTrigger) <- newTriggerEvent
      let
        loop = do
          line <- getLine
          () <- atomically $ writeTVar var line
          () <- inputTrigger ()
          loop

      eCons <- performEvent $ liftIO (readTVarIO var) <$ eInput
      dList <- mkListF eCons
      let
        dFlattened = distributeF dList

      performEvent_ $ liftIO . print . staticList <$> updated dFlattened

      void . liftIO $ forkIO loop
