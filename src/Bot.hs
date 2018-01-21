{-# LANGUAGE LambdaCase #-}
module Bot
        -- ( bot
        -- )
    where

import Vindinium
import BotHelper
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Debug.Trace


type Strategy a = State -> Maybe a

-- | Tries all strategies in order
tryAll :: [Strategy a] -> Strategy a
tryAll [] _ = Nothing
tryAll (b:bs) st
  | x@(Just _) <- b st = x
  | otherwise = tryAll bs st

-- | Convert a strategy to a bot - a bot with a plan!
runStrategy :: Strategy Dir -> Bot -> Bot
runStrategy strat fallback st
  | Just d <- strat st = return d
  | otherwise = fallback st

getState :: Strategy State
getState = Just

---------------------

-- | Determines if a tile is passable
isPassable :: Tile -> Bool
isPassable = \case FreeTile   -> True
                   HeroTile _ -> True
                   _          -> False
  -- TODO: Heroes are only passable when they can be killed

-- | Moves the player towards a given point
moveTowards :: Pos -> Strategy Dir
moveTowards p st = do
  let pos  = myPos st
      path = pathTo (board st) (const True) pos p

  tu@(h:_) <- turtlePath' pos <$> path
  return h

goToTile :: (Tile -> Bool) -> Strategy Dir
goToTile f st = do
  let ms = findTiles f $ board st
  x <- closestPath (board st) isPassable (myPos st) $ map fst ms
  tu@(h:_) <- pure $ turtlePath' (myPos st) x
  return h

---------------------

-- | A bot that doesn't do anything
idle :: Bot
idle = const $ return Stay

-- | Main bot
bot :: Bot
bot st = do
  liftIO $ print $ myHp st
  runStrategy strat idle st

-- | Main strategy
strat :: Strategy Dir
strat = tryAll
  [ attackKillableNeighbor
  , drinkLowHealth 20
  , drinkNextToTavern 70
  , stealMine
  ]

-- | Attack players next to you - given a reason
attackNeighbor :: (Hero -> Bool) -> Strategy Dir
attackNeighbor f st = do
  [HeroTile i] <- pure . filter isTileHero . map snd . neighbors (board st) $ myPos st
  h <- heroById (game st) i
  guard $ f h
  moveTowards (heroPos h) st
  -- TODO: Logic for the multiple players case

-- | Attack players next to you if you can actually beat them
attackKillableNeighbor :: Strategy Dir
attackKillableNeighbor st
  = attackNeighbor (fst . simFight (myHp st) . heroLife) st
  -- TODO: We might be standing at, or next to his spawn

-- | Get to the nearest tavern as quickly as possible and get a drink!
goDrink :: Strategy Dir
goDrink = goToTile (== TavernTile)

-- | Goes to the closest mine satisfying a condition
goMine :: (Tile -> Bool) -> Strategy Dir
goMine f = goToTile (\t -> isTileMine t && f t)

-- | Go to the closest hero satisfying a condition, that isn't yourself
goHero :: (Tile -> Bool) -> Strategy Dir
goHero f st = goToTile (\t -> isTileHero t && (\(HeroTile i) -> i /= myId st) t && f t) st

-- | Goes to the nearest tavern to get a drink if health is below some limit
drinkLowHealth :: Integer -> Strategy Dir
drinkLowHealth lim st = do
  guard $ myHp st <= lim
  goDrink st

-- | Might aswel grab a quick drink while we're here?
drinkNextToTavern :: Integer -> Strategy Dir
drinkNextToTavern lim st = do
  guard $ myHp st <= lim
  let ns = filter ((== TavernTile).snd) $ neighbors (board st) (myPos st)
  guard . not $ null ns
  moveTowards (fst $ head ns) st

-- | Go to a mine that you don't own
stealMine :: Strategy Dir
stealMine st = flip goMine st
             $ \case MineTile (Just i) -> i /= myId st
                     _              -> True
