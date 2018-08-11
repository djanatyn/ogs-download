{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module OGS.API.Fetch where

import           OGS.API.Games.Types
import           OGS.API.Types

import           Data.Aeson

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

-- https://online-go.com/api/v1/players/435842/games/

type URL = String

api :: URL
api = "https://online-go.com/api/v1/"

playerGames :: PlayerID -> URL
playerGames id = api ++ "players/" ++ (show id) ++ "/games/"

fetchGames :: PlayerID -> IO GamesResponse
fetchGames id = do
  manager  <- newManager tlsManagerSettings
  response <- parseRequest (playerGames id) >>= httpJSON

  return $ getResponseBody response

djanatyn :: PlayerID
djanatyn = 435842

djanatynGames :: IO [Game]
djanatynGames = do
    response <- fetchGames djanatyn
    return $ games response
