module Bot
        ( bot
        )
    where

import Vindinium
import BotHelper

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

bot :: Bot
bot st = do
  let board = gameBoard $ stateGame st
      hp    = heroPos $ stateHero st
      path  = pathTo board (const True) hp (Pos 0 0)
      tpath = turtlePath' hp <$> path

  liftIO $ do
    print hp
    print path
    print tpath

  return Stay

