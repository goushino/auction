{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, newTVarIO)
import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, encode)
import qualified Web.Scotty as Scotty
import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.HTTP.Types (badRequest400)
import Data.Aeson.TH (deriveJSON)
import qualified Network.Wreq as Wreq
import Data.String (IsString)
import Control.Exception.Safe
import Control.Lens ((^?))
import Control.Concurrent

data Counter a where
  Add :: Int -> Counter Int
  Reset :: Counter ()

newtype CounterState = CounterState {counter :: TVar Int}

evalCounterServer :: CounterState -> Counter a -> IO a
evalCounterServer state (Add n) = atomically $ do
  m <- readTVar (counter state)
  writeTVar (counter state) (m + n)
  return (m + n)
evalCounterServer state Reset = atomically $ writeTVar (counter state) 0

newtype Port = Port Int deriving (Num, Show)

jsonServer :: (FromJSON request, ToJSON response) => Port -> (request -> IO response) -> IO ()
jsonServer (Port port) requestHandler = Scotty.scotty port $ do
  Scotty.post "/api" $ do
    reqBody <- Scotty.body
    case decode reqBody of
      Just request -> do
        response <- liftIO $ requestHandler request
        Scotty.json response
      Nothing -> Scotty.status badRequest400

data CounterRequest = Add' Int | Reset'
data CounterResponse = Add'' Int | Reset'' ()

deriveJSON defaultOptions ''CounterRequest
deriveJSON defaultOptions ''CounterResponse

newCounterState :: Int -> IO CounterState
newCounterState n = do
  tv <- newTVarIO n
  return $ CounterState tv

jsonApiRequestHandler :: CounterState -> CounterRequest -> IO CounterResponse
jsonApiRequestHandler state (Add' n) = Add'' <$> evalCounterServer state (Add n)
jsonApiRequestHandler state Reset' = Reset'' <$> evalCounterServer state Reset

counterServer :: Port -> IO ()
counterServer port = do
  state <- newCounterState 0
  jsonServer port (jsonApiRequestHandler state)

newtype Host = Host String deriving (IsString, Show)

newtype CounterError = CounterError String deriving (Show, Typeable)
instance Exception CounterError

callCounterApi :: Host -> Port -> CounterRequest -> IO CounterResponse
callCounterApi (Host host) (Port p) request = do
  response <- Wreq.post ("http://" <> host <> ":" <> show p <> "/api") (encode @CounterRequest request)
  case response ^? Wreq.responseBody of
    Just body -> case decode @CounterResponse body of
      Just myResponse -> return myResponse
      Nothing -> throwIO (CounterError "Response decode error")
    Nothing -> throwIO (CounterError "network error")

evalCounterClient :: Host -> Port -> Counter a -> IO a
evalCounterClient host port (Add n) =
  let unwrap (Add'' x) = x
  in unwrap <$> callCounterApi host port (Add' n)
evalCounterClient host port Reset =
  let unwrap (Reset'' x) = x
  in unwrap <$> callCounterApi host port Reset'

main :: IO ()
main = do
  let port = 4000
  let host = "localhost"
  let add n = evalCounterClient host port (Add n)
  let reset = evalCounterClient host port Reset

  _ <- forkIO $ counterServer port

  threadDelay 1000000
  print =<< add 1
  print =<< add 1
  print =<< reset
  print =<< add 1
