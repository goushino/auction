{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Auction.Types where

import Control.Concurrent.STM
import Control.Lens (makeFields, (^.))
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Time
import Data.UUID (UUID, fromString, toString)
import Data.UUID.V4 as UUID.V4
import Prelude hiding (id)
import Control.Exception.Safe

newtype ItemId = ItemId UUID deriving (Show, Eq, Ord)

newtype UserId = UserId UUID deriving (Eq, Ord, Show, Hashable)

newtype Money = Money Int deriving (Show, Read, Eq, Ord, Num)

newtype AuctionItemId = AuctionItemId UUID deriving (Show, Eq, Ord)

data Term = Term {startTime :: UTCTime, endTime :: UTCTime} deriving (Show)

type Price = Money

data NewItem = NewItem
  { _newItemName :: String,
    _newItemDescription :: String
  }
  deriving (Show)

makeFields ''NewItem

data Item = Item
  { _itemId :: ItemId,
    _itemName :: String,
    _itemDescription :: String
  }
  deriving (Show)

makeFields ''Item

newtype Inventory = Inventory [Item] deriving (Show)

data NewUser = NewUser
  { _newUserName :: String,
    _newUserMoney :: Money
  }
  deriving (Show)

makeFields ''NewUser

data User = User
  { _userId :: UserId,
    _userName :: String,
    _userInventory :: Inventory,
    _userMoney :: Money
  }
  deriving (Show)

makeFields ''User

buildUser :: NewUser -> IO User
buildUser newUser = do
  uuid <- UUID.V4.nextRandom
  let userId = UserId uuid
  let emptyInventory = Inventory []
  let user = User userId (newUser ^. name) emptyInventory (newUser ^. money)
  return user

addItem :: Inventory -> Item -> Inventory
addItem (Inventory items) item = Inventory (item : items)

buildItem :: NewItem -> IO Item
buildItem item = do
  uuid <- UUID.V4.nextRandom
  return $ Item (ItemId uuid) (item ^. name) (item ^. description)

checkInTerm :: UTCTime -> Term -> Bool
checkInTerm time term = startTime term <= time && time < endTime term

newAuctionItemId :: IO AuctionItemId
newAuctionItemId = AuctionItemId <$> UUID.V4.nextRandom

userIdToString :: UserId -> String
userIdToString (UserId uuid) = toString uuid

stringToUserId :: String -> Maybe UserId
stringToUserId str = UserId <$> fromString str

data AuctionItem' = AuctionItem'
  { _auctionItem'AuctionItemId :: AuctionItemId,
    _auctionItem'Seller :: TVar User,
    _auctionItem'CurrentPrice :: Price,
    _auctionItem'CurrentUser :: Maybe (TVar User),
    _auctionItem'AuctionTerm :: Term,
    _auctionItem'AuctionTargetItem :: Item
  }

makeFields ''AuctionItem'

data AuctionItem = AuctionItem
  { _auctionItemAuctionItemId :: AuctionItemId,
    _auctionItemSellerId :: UserId,
    _auctionItemCurrentPrice :: Price,
    _auctionItemCurrentUserId :: Maybe UserId,
    _auctionItemAuctionTerm :: Term,
    _auctionItemAuctionTargetItem :: Item
  }
  deriving (Show)

makeFields ''AuctionItem

toAuctionItem :: AuctionItem' -> IO AuctionItem
toAuctionItem tAuctionItem = do
  sellUser <- readTVarIO (tAuctionItem ^. seller)
  let mtCurrentUser = tAuctionItem ^. currentUser
  mCurrentUserId <- case mtCurrentUser of
    Just tCurrentUser -> do
      user <- readTVarIO tCurrentUser
      return $ Just (user ^. id)
    Nothing -> return Nothing
  return $
    AuctionItem
      { _auctionItemAuctionItemId = tAuctionItem ^. auctionItemId,
        _auctionItemSellerId = sellUser ^. id,
        _auctionItemCurrentPrice = tAuctionItem ^. currentPrice,
        _auctionItemCurrentUserId = mCurrentUserId,
        _auctionItemAuctionTerm = tAuctionItem ^. auctionTerm,
        _auctionItemAuctionTargetItem = tAuctionItem ^. auctionTargetItem
      }

data Auction a where
  RegisterUser :: NewUser -> Auction UserId
  CheckUser :: UserId -> Auction User
  RegisterItem :: UserId -> NewItem -> Auction ItemId
  SellToAuction :: UserId -> Item -> Term -> Price -> Auction ()
  ViewAuctionItem :: Auction (Maybe AuctionItem)
  Bid :: UserId -> Price -> Auction ()

data AuctionRequest
  = ViewAuctionItemReq
  | RegissterUserReq NewUser
  | CheckUserReq UserId
  | RegisterItemReq UserId NewItem
  | BidReq UserId Price
  | SellToAuctionReq UserId Item Term Price

data AuctionResponse
  = ViewAuctionItemRes (Maybe AuctionItem)
  | RegisterUserRes UserId
  | CheckUserRes User
  | RegisterItemRes ItemId
  | BidRes ()
  | SellToAuctionRes ()

newtype AuctionServerRequest = AuctionServerRequest AuctionRequest
data AuctionServerResponse = AuctionOk AuctionResponse | AuctionErr AuctionException

data AuctionState = AuctionState
  { registeredUsers :: TVar (M.HashMap UserId (TVar User)),
    currentAuctionItem :: TMVar AuctionItem'
  }

newAuctionState :: IO AuctionState
newAuctionState = do
  emptyUsers <- newTVarIO M.empty
  AuctionState emptyUsers <$> newEmptyTMVarIO

data CheckResult = Ok | Err String

data AuctionException
  = NoAuctionItem | LowPrice | OutOfTerm | UserNotFound
  | InvalidTerm | InvalidFirstPrice | AuctionItemAlreadyExist
  | InvalidUserUpdate | ItemNotFound | NoEnoughMoney | BadData String
  | UnknownError | NetworkError | DecodeError
  deriving (Show, Typeable)
instance Exception AuctionException

deriveJSON defaultOptions ''Money
deriveJSON defaultOptions ''AuctionItemId
deriveJSON defaultOptions ''AuctionItem
deriveJSON defaultOptions ''ItemId
deriveJSON defaultOptions ''NewItem
deriveJSON defaultOptions ''Item
deriveJSON defaultOptions ''Term
deriveJSON defaultOptions ''NewUser
deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''Inventory
deriveJSON defaultOptions ''UserId

-- deriveJSON defaultOptions ''UUID

deriveJSON defaultOptions ''AuctionRequest
deriveJSON defaultOptions ''AuctionResponse
deriveJSON defaultOptions ''AuctionServerRequest
deriveJSON defaultOptions ''AuctionServerResponse
deriveJSON defaultOptions ''AuctionException

