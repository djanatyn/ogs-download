{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module OGS.API.Fetch where

import           OGS.API.Games.Types
import           OGS.API.Types

import Control.Monad

import           Data.Aeson

import qualified Data.Text     as T

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

import Control.Concurrent (threadDelay)

import Data.Maybe
import Pipes

-- https://online-go.com/api/v1/players/435842/games/

type URL = String

delay :: Int
delay = (1000000 * 5) -- 5 seconds

api :: URL
api = "https://online-go.com/api/v1/"

playerGames :: PlayerID -> URL
playerGames id = api ++ "players/" ++ (show id) ++ "/games/"

fetchPage :: URL -> IO GameListResponse
fetchPage url = do
    manager  <- newManager tlsManagerSettings
    response <- parseRequest url >>= httpJSON

    return $ getResponseBody response

fetchPages :: URL -> Producer GameListResponse IO ()
fetchPages url = do
  page <- lift $ fetchPage url
  yield page

  lift $ threadDelay delay
  case (nextPage page) of
    Just nextURL -> fetchPages nextURL
    Nothing      -> return ()

responsesToGames :: Pipe GameListResponse Game IO ()
responsesToGames = forever $ do
  response <- await
  mapM_ yield $ games response

fetchGames :: PlayerID -> Producer Game IO ()
fetchGames id = (fetchPages $ playerGames id) >-> responsesToGames

-- runEffect $ for (fetchGames 435842) (lift . putStrLn . show)
