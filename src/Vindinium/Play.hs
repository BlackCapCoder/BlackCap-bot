{-# LANGUAGE LambdaCase #-}
module Vindinium.Play
        ( playTraining
        , playArena
        )
    where

import Vindinium.Types
import Vindinium.Api
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import Control.Monad
import System.Process (spawnCommand)

playTraining :: Maybe Int -> Maybe Board -> Bot -> Vindinium State
playTraining mt mb b = do
  st <- startTraining mt mb
  beforeGame st
  playLoop b st

playArena :: Bot -> Vindinium State
playArena b = do
  st <- startArena
  beforeGame st
  playLoop b st

playLoop :: Bot -> State -> Vindinium State
playLoop bot state = do
  liftIO . putStrLn . drawBoard . gameBoard $ stateGame state

  if (gameFinished . stateGame) state
      then return state
      else do
          newState <- bot state >>= move state
          playLoop bot newState

beforeGame :: State -> Vindinium ()
beforeGame st = do
  -- liftIO . showGame $ stateViewUrl st
  return ()

----------------

-- Render the game as ASCII art
drawBoard :: Board -> String
drawBoard (Board s ts)
  = unlines $ chunksOf s $ map f ts
  where f = \case
          FreeTile    -> '.'
          WoodTile    -> '#'
          TavernTile  -> 't'
          MineTile _  -> 'm'
          HeroTile (HeroId i) -> head $ show i

-- Open the game in a browser on a remote monitor
showGame url = void $ spawnCommand ("ssh blackcap@158.36.81.46 'DISPLAY=:0 chromium " ++ T.unpack url ++ "'")
