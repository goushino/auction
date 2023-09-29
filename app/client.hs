{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- import qualified Control.Monad.Catch as Catch (MonadCatch (..), MonadThrow (..))

-- import qualified System.Console.Haskeline.MonadException as Haskeline (catch)

import Auction.Client
import Auction.Types
import Control.Exception.Safe (Exception, SomeException, catch)
import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..), maybeToExceptT)
import Data.List (elemIndex, isPrefixOf)
import Data.String (IsString)
import Data.Time (addUTCTime)
import Data.Time.Clock (getCurrentTime)
import Safe (atMay)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Read (readMaybe)
import Prelude hiding (print, putStr, putStrLn)
import qualified Prelude (print, putStr, putStrLn)

putStrLn :: MonadIO m => String -> m ()
putStrLn x = liftIO $ Prelude.putStrLn x

putStr :: MonadIO m => String -> m ()
putStr x = liftIO $ Prelude.putStr x

print :: (MonadIO m, Show s) => s -> m ()
print x = liftIO $ Prelude.print x

-- instance Catch.MonadThrow (InputT IO) where
-- throwM e = liftIO $ throwIO e

-- instance Catch.MonadCatch (InputT IO) where
--   catch act handler = Haskeline.catch act $ \e ->
--     if isSyncException e
--       then handler e
--       else throwIO e

type AuctionClient = AuctionSessionT (InputT IO)

newtype ClientError = ClientError String deriving (Show, IsString)

instance Exception ClientError

toExcept :: e -> InputT IO (Maybe a) -> ExceptT e AuctionClient a
toExcept err act = maybeToExceptT err $ MaybeT $ lift act

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      showHelp
      mCommand <- getCommand "> "
      case mCommand of
        Nothing -> loop
        Just (command, rest) -> case command of
          "/quit" -> return ()
          "/signup" ->
            if null rest
              then putStrLn "/signup <name>" >> loop
              else do
                uid <- registerUser rest 10000
                putStrLn $ "Your UserId: " ++ userIdToString uid
                runAuctionSession (ClientSession uid) auctionRepl
                loop
          "/signin" -> do
            case stringToUserId rest of
              Just uid -> do
                runAuctionSession (ClientSession uid) auctionRepl
                loop
              Nothing -> loop
          _ -> loop

    showHelp =
      putStrLn $
        mconcat
          [ "/quit: Quit REPL\n",
            "/signup <name>: Signup as <name>\n",
            "/signin <user_id>: Signin with <user_id>"
          ]

getCommand :: String -> InputT IO (Maybe (String, String))
getCommand prompt = do
  mInput <- getInputLine prompt
  case mInput of
    Nothing -> return Nothing
    Just _input ->
      let input = dropWhile (== ' ') _input
       in if "/" `isPrefixOf` input
            then case elemIndex ' ' input of
              Just index -> case splitAt index input of
                (command, rest) -> return $ Just (command, dropWhile (== ' ') rest)
              Nothing -> return $ Just (input, "")
            else return Nothing

getInt :: String -> InputT IO (Maybe Int)
getInt prompt = do
  mInput <- getInputLine prompt
  case mInput of
    Nothing -> return Nothing
    Just input -> case readMaybe $ dropWhile (== ' ') input of
      Nothing -> return Nothing
      Just num -> return $ Just num

auctionRepl :: AuctionClient ()
auctionRepl = loop `catch` \e -> print (e :: SomeException)
  where
    loop = do
      showHelp
      user <- checkUser
      mCommand <- lift $ getCommand $ (user ^. name) ++ "> "
      case mCommand of
        Nothing -> loop
        Just (command, _rest) -> case command of
          "/signout" -> return ()
          "/status" -> checkUserRepl >> loop
          "/check" -> checkItemRepl >> loop
          "/register" -> registerItemRepl >> loop
          "/sell" -> sellToAuctionRepl >> loop
          "/bid" -> bidRepl >> loop
          _ -> loop
    showHelp =
      putStrLn $
        mconcat
          [ "/signout: Sign out\n",
            "/status: Show user's status\n",
            "/check: Show current auction item\n",
            "/register: register item to sell\n",
            "/sell: sell item to auction\n",
            "/bid: bid auction item"
          ]

checkUserRepl :: AuctionClient ()
checkUserRepl = repl `catch` \e -> print (e :: SomeException)
  where
    repl = do
      user <- checkUser
      print user

checkItemRepl :: AuctionClient ()
checkItemRepl = repl `catch` \e -> print (e :: SomeException)
  where
    repl = do
      mAuctionItem <- viewAuctionItem
      case mAuctionItem of
        Just auctionItem -> print auctionItem
        Nothing -> putStrLn "No AuctionItem now"

registerItemRepl :: AuctionClient ()
registerItemRepl = repl `catch` \e -> print (e :: SomeException)
  where
    repl = do
      ei <- runExceptT @ClientError $ do
        nm <- toExcept "canceled" (getInputLine "item name: ")
        when (null nm) $ throwE "empty name"
        desc <- toExcept "canceled" (getInputLine "item desc: ")
        when (null desc) $ throwE "empty description"
        itemId <- lift $ registerItem (NewItem nm desc)
        return (nm, itemId)
      case ei of
        Right (nm, ItemId itemId) -> putStrLn $ "Registered item(" ++ nm ++ "." ++ show itemId ++ ")"
        Left (ClientError msg) -> putStrLn msg

sellToAuctionRepl :: AuctionClient ()
sellToAuctionRepl = repl `catch` \e -> print (e :: SomeException)
  where
    repl = do
      user <- checkUser
      let Inventory items = user ^. inventory
      let revItems = reverse items
      forM_ (zip revItems [1 :: Int ..]) $ \(item, ix) -> do
        putStrLn $ show ix ++ ": " ++ item ^. name

      ei <- runExceptT @ClientError $ do
        index <- toExcept "invalid input" $ getInt "select one: "
        item <- toExcept "invalid index" $ return $ revItems `atMay` (index -1)
        sec <- toExcept "invalid input" $ getInt "length(sec): "
        when (sec <= 0) $ throwE "invalid length"
        firstPrice <- toExcept "invalid input" $ getInt "first price: "
        when (firstPrice < 0) $ throwE "invalid first price"
        currentTime <- liftIO getCurrentTime
        let et = addUTCTime (fromIntegral sec) currentTime
        let term = Term currentTime et
        lift $ sellToAuction item term (fromIntegral firstPrice)
        return item
      case ei of
        Right item -> putStrLn $ "Sold " ++ (item ^. name) ++ " to auction"
        Left (ClientError msg) -> putStrLn msg

bidRepl :: AuctionClient ()
bidRepl = repl `catch` \e -> print (e :: SomeException)
  where
    repl = do
      mInput <- lift $ getInputLine "bid price: "
      case mInput of
        Nothing -> return ()
        Just input -> case readMaybe input of
          Nothing -> putStrLn "invalid input"
          Just (price :: Int) -> do
            bid (fromIntegral price)
            putStrLn $ "Bid at " ++ show price
