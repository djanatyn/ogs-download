{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module OGS.API.Types
    ( Game(Game)
    , GamesResponse(GamesResponse)
    , games
    , PlayerID
    ) where

import           Data.Aeson

type PlayerID = Integer

data Game = Game
    { id    :: Integer
    , name  :: String
    , black :: PlayerID
    , white :: PlayerID
    } deriving (Eq, Show, Ord)

data GamesResponse = GamesResponse
    { games :: [Game]
    } deriving (Eq, Show)

instance FromJSON Game where
    parseJSON = withObject "game" $ \o -> do
      id    <- o .: "id"
      name  <- o .: "name"
      black <- o .: "players" >>= (.: "black") >>= (.: "id")
      white <- o .: "players" >>= (.: "white") >>= (.: "id")

      return Game{..}

instance FromJSON GamesResponse where
    parseJSON = withObject "response" $ \o -> do
      games <- o .: "results"

      return GamesResponse{..}
