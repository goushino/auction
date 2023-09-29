{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auction.Server (jsonServer, Port (..), auctionService, facilitator) where

import Auction.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Time.Clock
import Network.HTTP.Types (badRequest400)
import qualified Web.Scotty as Scotty
import Prelude hiding (id)

newtype Port = Port Int deriving (Show)

jsonServer :: (FromJSON request, ToJSON response) => Port -> (request -> IO response) -> IO ()
jsonServer (Port port) requestHandler = Scotty.scotty port $ do
  Scotty.post "/api" $ do
    reqBody <- Scotty.body
    case decode reqBody of
      Just request -> do
        response <- liftIO $ requestHandler request
        Scotty.json response
      Nothing -> Scotty.status badRequest400

auctionService :: AuctionState -> AuctionServerRequest -> IO AuctionServerResponse
auctionService auctionState (AuctionServerRequest request) =
  requestHandler
    `catch` \(e :: AuctionException) ->
      return (AuctionErr e)
        `catch` \(_ :: SomeException) -> return (AuctionErr UnknownError)
  where
    requestHandler = case checkInput request of
      Err msg -> return $ AuctionErr (BadData msg)
      Ok -> case request of
        ViewAuctionItemReq -> do
          result <- evalAuction auctionState ViewAuctionItem
          return $ AuctionOk (ViewAuctionItemRes result)
        RegisterUserReq newUser -> do
          result <- evalAuction auctionState (RegisterUser newUser)
          return $ AuctionOk (RegisterUserRes result)
        CheckUserReq uid -> do
          result <- evalAuction auctionState (CheckUser uid)
          return $ AuctionOk (CheckUserRes result)
        RegisterItemReq uid item -> do
          result <- evalAuction auctionState (RegisterItem uid item)
          return $ AuctionOk (RegisterItemRes result)
        BidReq uid price -> do
          result <- evalAuction auctionState (Bid uid price)
          return $ AuctionOk (BidRes result)
        SellToAuctionReq uid item term price -> do
          result <- evalAuction auctionState (SellToAuction uid item term price)
          return $ AuctionOk (SellToAuctionRes result)

getUser :: AuctionState -> UserId -> STM (Maybe (TVar User))
getUser state uid = do
  userMap <- readTVar $ registeredUsers state
  return $! M.lookup uid userMap

getLoginUser :: AuctionState -> UserId -> IO (TVar User)
getLoginUser state uid = do
  atomically (getUser state uid) >>= \case
    Just tUser -> return tUser
    Nothing -> throwIO UserNotFound

updateUser :: TVar User -> (User -> User) -> STM ()
updateUser tUser change = do
  oldUser <- readTVar tUser
  let changedUser = change oldUser
  if changedUser ^. id == oldUser ^. id
    then writeTVar tUser changedUser
    else throwIO InvalidUserUpdate

registerUser :: AuctionState -> User -> IO ()
registerUser state user = atomically $ do
  let users = registeredUsers state
  tUser <- newTVar user
  modifyTVar' users $ M.insert (user ^. id) tUser

checkInput :: AuctionRequest -> CheckResult
checkInput ViewAuctionItemReq = Ok
checkInput (RegisterUserReq user) = if not (null (user ^. name)) then Ok else Err "User name is required"
checkInput (CheckUserReq _uid) = Ok
checkInput (RegisterItemReq _uid item) =
  if not (null (item ^. name)) && not (null (item ^. description))
    then Ok
    else Err "Item name and description should not be empty"
checkInput (BidReq _uid price) =
  if price > 0
    then Ok
    else Err "Bidding price must be positive"
checkInput (SellToAuctionReq _uid _item term price) =
  if startTime term < endTime term
    then
      if price >= 0
        then Ok
        else Err "First price must be non-negative"
    else Err "Invalid term"

evalAuction :: AuctionState -> Auction a -> IO a
evalAuction state (RegisterUser newUser) = do
  user <- buildUser newUser
  registerUser state user
  return $ user ^. id
evalAuction state (RegisterItem uid ni) = do
  tUser <- getLoginUser state uid
  item <- buildItem ni
  atomically $ addItemToUser tUser item
evalAuction state ViewAuctionItem = do
  mtAuctionItem <- atomically $ tryReadTMVar (currentAuctionItem state)
  case mtAuctionItem of
    Just tAuctionItem -> do
      auctionItem <- toAuctionItem tAuctionItem
      return $ Just auctionItem
    Nothing -> return Nothing
evalAuction state (CheckUser uid) = getLoginUser state uid >>= readTVarIO
evalAuction state (SellToAuction uid item term price) = do
  tUser <- getLoginUser state uid
  currentTime <- getCurrentTime
  if startTime term < endTime term && currentTime < endTime term
    then
      if price < 0
        then throwIO InvalidFirstPrice
        else do
          _ <- putItemToAuction state tUser item term price
          return ()
    else throwIO InvalidTerm
evalAuction state (Bid uid bidPrice) = do
  tUser <- getLoginUser state uid
  currentTime <- getCurrentTime
  atomically $ do
    user <- readTVar tUser
    if user ^. money < bidPrice
      then throwIO NoEnoughMoney
      else do
        mAuctionItem <- tryTakeTMVar $ currentAuctionItem state
        case mAuctionItem of
          Nothing -> throwIO NoAuctionItem
          Just auctionItem ->
            if checkInTerm currentTime (auctionItem ^. auctionTerm)
              then
                if bidPrice <= (auctionItem ^. currentPrice)
                  then throwIO LowPrice
                  else do
                    let auctionItem' =
                          auctionItem & currentPrice .~ bidPrice
                            & currentUser .~ Just tUser
                    putTMVar (currentAuctionItem state) auctionItem'
              else throwIO OutOfTerm

addItemToUser :: TVar User -> Item -> STM ItemId
addItemToUser tUser item = do
  updateUser tUser (\user -> user & inventory .~ addItem (user ^. inventory) item)
  return $ item ^. id

putItemToAuction :: AuctionState -> TVar User -> Item -> Term -> Price -> IO AuctionItem'
putItemToAuction state tUser item term price = do
  aiid <- newAuctionItemId
  atomically $ do
    user <- readTVar tUser
    let Inventory inventoryItems = user ^. inventory
    if item ^. id `elem` fmap (^. id) inventoryItems
      then do
        let inventoryItems' =
              filter (\item' -> item' ^. id /= item ^. id) inventoryItems
        let user' = user & inventory .~ Inventory inventoryItems'
        writeTVar tUser user'
        mAuctionItem <- tryTakeTMVar $ currentAuctionItem state
        case mAuctionItem of
          Just _ -> throwIO AuctionItemAlreadyExist
          Nothing -> do
            let tAuctionItem = AuctionItem' aiid tUser price Nothing term item
            putTMVar (currentAuctionItem state) tAuctionItem
            return tAuctionItem
      else throwIO ItemNotFound

facilitator :: TQueue T.Text -> AuctionState -> IO ()
facilitator queue state = loop
  where
    loop = do
      threadDelay $ 1000 * 1000 -- 1sec
      handleFinishedAuctionItem queue state
      loop

handleFinishedAuctionItem :: TQueue T.Text -> AuctionState -> IO ()
handleFinishedAuctionItem queue state = do
  currentTime <- getCurrentTime
  atomically $ do
    mAuctionItem <- tryReadTMVar (currentAuctionItem state)
    case mAuctionItem of
      Nothing -> writeTQueue queue "FACILITATOR: Auction doesn't hold"
      Just auctionItem ->
        if currentTime < endTime (auctionItem ^. auctionTerm)
          then writeTQueue queue "FACILITATOR: Auction holds"
          else do
            void $ takeTMVar (currentAuctionItem state)
            case auctionItem ^. currentUser of
              Just tWinner -> do
                updateUser
                  tWinner
                  ( \winner ->
                      winner
                        & inventory
                          .~ addItem
                            (winner ^. inventory)
                            (auctionItem ^. auctionTargetItem)
                        & money
                          .~ (winner ^. money) - (auctionItem ^. currentPrice)
                  )
                let tSeller = auctionItem ^. seller
                updateUser
                  tSeller
                  ( \sellUser ->
                      sellUser & money
                        .~ (sellUser ^. money) + (auctionItem ^. currentPrice)
                  )
                writeTQueue queue "FACILITATOR: Auction finished successfully"
              Nothing -> do
                let tSeller = auctionItem ^. seller
                void $ addItemToUser tSeller (auctionItem ^. auctionTargetItem)
                writeTQueue queue "FACILITATOR: Aucton fiinished with no bidder"
