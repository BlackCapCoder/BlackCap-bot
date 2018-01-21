{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vindinium.Types
        ( Vindinium
        , runVindinium
        , asks
        , Settings (..)
        , Key (..)
        , Bot
        , State (..)
        , GameId (..)
        , Game (..)
        , HeroId (..)
        , Hero (..)
        , Board (..)
        , Tile (..)
        , Pos (..)
        , Dir (..)
        , Path
        , Map
        )
    where

import Data.Text (Text)

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (MonadIO)

import Data.Hashable (Hashable (..))
import PlaneZipper

newtype Key = Key Text deriving (Show, Eq)

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

newtype Vindinium a = Vindinium { unVindinium :: ReaderT Settings IO a }
    deriving (Functor, Applicative, Monad, MonadReader Settings, MonadIO)

runVindinium :: Settings -> Vindinium a -> IO a
runVindinium s = flip runReaderT s . unVindinium

type Bot = State -> Vindinium Dir

data State = State {
    stateGame    :: Game
  , stateHero    :: Hero
  , stateToken   :: Text
  , stateViewUrl :: Text
  , statePlayUrl :: Text
} deriving (Show, Eq)

newtype GameId = GameId Text
    deriving (Show, Eq)

data Game = Game {
    gameId       :: GameId
  , gameTurn     :: Integer
  , gameMaxTurns :: Integer
  , gameHeroes   :: [Hero]
  , gameBoard    :: Board
  , gameFinished :: Bool
} deriving (Show, Eq)

newtype HeroId = HeroId Int
    deriving (Show, Eq, Ord)

type Path = [Dir]

data Hero = Hero {
    heroPath      :: Path
  , heroId        :: HeroId
  , heroName      :: Text
  , heroUserId    :: Maybe Text
  , heroElo       :: Maybe Integer
  , heroPos       :: Pos
  , heroLife      :: Integer
  , heroGold      :: Integer
  , heroMineCount :: Integer
  , heroSpawnPos  :: Pos
  , heroCrashed   :: Bool
} deriving (Show, Eq)

data Board = Board {
    boardSize  :: Int
  , boardTiles :: [Tile]
} deriving (Show, Eq)

data Tile = FreeTile
          | WoodTile
          | TavernTile
          | HeroTile HeroId
          | MineTile (Maybe HeroId)
    deriving (Show, Eq, Ord)

data Pos = Pos {
    posX :: Int
  , posY :: Int
} deriving (Show, Eq, Ord)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq, Enum, Ord)

instance Hashable Pos where
  hashWithSalt = undefined
  hash (Pos x y) = hash $ (0.5 :: Float) * fromIntegral (x+y) * fromIntegral (x+y+1) + fromIntegral y

instance Num Pos where
  (Pos x1 y1) + (Pos x2 y2) = Pos (x1+x2) (y1+y2)
  (Pos x1 y1) * (Pos x2 y2) = Pos (x1*x2) (y1*y2)
  (Pos x1 y1) - (Pos x2 y2) = Pos (x1-x2) (y1-y2)
  abs (Pos x1 y1) = Pos (abs x1) (abs y1)
  signum = undefined
  fromInteger = undefined

type Map = Z Tile
