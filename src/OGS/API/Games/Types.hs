{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module OGS.API.Games.Types
    ( Game(Game)
    , GamesResponse(GamesResponse)
    , games
    , nextPage
    ) where

import           Data.Aeson
import qualified Data.Text     as T
import           OGS.API.Types

data Game = Game
    { id    :: Integer
    , name  :: T.Text
    , black :: PlayerID
    , white :: PlayerID
    , url   :: String
    } deriving (Eq, Show, Ord)

data GamesResponse = GamesResponse
    { games    :: [Game]
    , nextPage :: Maybe String
    } deriving (Eq, Show)

instance FromJSON Game where
    parseJSON = withObject "game" $ \o -> do
      id    <- o .: "id"
      name  <- o .: "name"
      black <- o .: "players" >>= (.: "black") >>= (.: "id")
      white <- o .: "players" >>= (.: "white") >>= (.: "id")
      url   <- o .: "related" >>= (.: "detail")

      return Game{..}

instance FromJSON GamesResponse where
    parseJSON = withObject "response" $ \o -> do
      games    <- o .: "results"
      nextPage <- o .:? "next"

      return GamesResponse{..}
