module Bot
        ( bot
        )
    where

import Vindinium
import BotHelper
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

type Strategy = State -> Maybe Dir

-- | Tries all strategies in order
tryAll :: [Strategy] -> Strategy
tryAll [] _ = Nothing
tryAll (b:bs) st
  | x@(Just _) <- b st = x
  | otherwise = tryAll bs st

-- | Convert a strategy to a bot - a bot with a plan!
runStrategy :: Strategy -> Bot -> Bot
runStrategy strat fallback st
  | Just d <- strat st = return d
  | otherwise = fallback st

-- | Moves the player towards a given point
moveTowards :: Pos -> Strategy
moveTowards p st = do
  let board = gameBoard $ stateGame st
      hp    = heroPos $ stateHero st
      path  = pathTo board (const True) hp (Pos 0 0)

  (h:_) <- turtlePath' hp <$> path
  return h

---------------------

-- | A bot that doesn't do anything
idle :: Bot
idle = const $ return Stay

-- | Main bot
bot :: Bot
bot = runStrategy strat idle

-- | Main strategy
strat :: Strategy
strat = tryAll
  [
  ]

