module Main where

import Prelude hiding (id)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Auction.Client
import Auction.Types
import Control.Lens ((^.))

main :: IO ()
main = do
  uid <- registerUser "bidderbot" 100000
  runAuctionSession (ClientSession uid) bidderbot

type AuctionBot = AuctionSessionT IO

bidderbot :: AuctionBot ()
bidderbot = loop
  where
    loop = do
      liftIO $ threadDelay $ 2 * 1000000
      user <- checkUser
      mAuctionItem <- viewAuctionItem
      case  mAuctionItem of
        Nothing -> loop
        Just auctionItem ->
          if auctionItem ^. currentUserId == Just (user ^. id)
            then loop
            else do
              bid (auctionItem ^. currentPrice + 100)
              loop
