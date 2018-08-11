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

-- https://online-go.com/api/v1/players/435842/games/

type URL = String

api :: URL
api = "https://online-go.com/api/v1/"

playerGames :: PlayerID -> URL
playerGames id = api ++ "players/" ++ (show id) ++ "/games/"

fetchPage :: URL -> IO GamesResponse
fetchPage url = do
    manager  <- newManager tlsManagerSettings
    response <- parseRequest url >>= httpJSON

    return $ getResponseBody response

fetchPages :: URL -> IO [GamesResponse]
fetchPages url = do
    page <- fetchPage url

    case (nextPage page) of
      Nothing   -> return [page]
      Just next -> do
        rest <- fetchPages next
        return $ [page] ++ rest

fetchGames :: PlayerID -> IO [Game]
fetchGames id = do
    responses <- fetchPages (playerGames id) 
    
    return $ join $ mapM games responses


djanatyn :: PlayerID
djanatyn = 435842
