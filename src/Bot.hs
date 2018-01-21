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
import Data.Maybe
import qualified Data.Set as S


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
  -- TODO: Tiles are only passable when they don't kill you

-- | Determines if a hero is killable
isKillable :: Integer -> Integer -> Bool
isKillable hp = fst . simFight hp

-- | Determines if a hero is hear anothers spawn
isNearSpawn :: Hero -> Hero -> Bool
isNearSpawn x h = taxicabDist (heroPos x) (heroSpawnPos h) < 2

-- | Determines if a hero is next to a tavern
isNearTavern :: Board -> Pos -> Bool
isNearTavern b p = any ((==TavernTile).snd) (neighbors b p)

-- | Determines if a hero is camping
isCamping :: Board -> Hero -> Hero -> Bool
isCamping b x h = isNearSpawn x h || isNearTavern b (heroPos h)

-- | Moves the player towards a given point
moveTowards :: Pos -> Strategy Dir
moveTowards p st = do
  (h:_) <- pathTo' (board st) (isPassable.snd) (myPos st) p
  return h

goToTile :: ((Pos, Tile) -> Bool) -> Strategy Dir
goToTile f st = do
  let ms = findTiles f $ board st
  (h:_) <- closestPath' (board st) (isPassable.snd) (myPos st) $ map fst ms
  return h

-- | Makes sure not to move within attack range of a non-killable player
safeGoToTile :: ((Pos, Tile) -> Bool) -> Strategy Dir
safeGoToTile f st = do
  let ms = findTiles f $ board st
  (h:_) <- closestPath' (board st) (\x -> (isPassable.snd) x && f' x) (myPos st) $ map fst ms
  return h
  where f' (p, t) | ts  <- filter isTileHero $ snd <$> attackRange (board st) p
                  , ts' <- filter (/= myId st) $ heroTileId <$> ts
                  , hs  <- fromJust . heroById (game st) <$> ts'
                  = not $ any (flip isKillable (myHp st) . heroLife) hs
    -- TODO: A tavern might save a path


-- | Get neighboring heroes
neighboringHeros :: (Hero -> Bool) -> Strategy [Hero]
neighboringHeros f st = do
  ts <- pure . filter isTileHero . map snd . neighbors (board st) $ myPos st
  let hs = filter f . catMaybes $ heroById (game st) . heroTileId <$> ts
  guard . not $ null hs
  return hs

-- | Get heroes in attack range
offensiveHeros :: (Hero -> Bool) -> Strategy [Hero]
offensiveHeros f st = do
  ts <- pure . filter isTileHero . map snd . attackRange (board st) $ myPos st
  let hs = filter f . catMaybes $ heroById (game st) . heroTileId <$> ts
  guard . not $ null hs
  return hs

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
  [ selfDefence
  , profitDefence
  , goDrinkLowHealth attackDamage
  -- , goDrinkLowHealthUnsafe attackDamage
  , drinkNextToTavern 70
  , flee
  , stealMine
  , goDrinkLowHealth 80
  ]

-- | Get to the nearest tavern as quickly as possible and get a drink!
goDrink :: Strategy Dir
goDrink = safeGoToTile ((== TavernTile).snd)

-- | Goes to the closest mine satisfying a condition
goMine :: (Tile -> Bool) -> Strategy Dir
goMine f = safeGoToTile (\(_,t) -> isTileMine t && f t)

-- | Go to the closest hero satisfying a condition, that isn't yourself
goHero :: (Tile -> Bool) -> Strategy Dir
goHero f st = safeGoToTile (\(_,t) -> isTileHero t && (\(HeroTile i) -> i /= myId st) t && f t) st

-- | Go to the nearest tavern and get a drink if health is below some limit
goDrinkLowHealth :: Integer -> Strategy Dir
goDrinkLowHealth lim st = do
  guard $ myHp st <= lim
  goDrink st

-- | We need a drink now! Never mind the risks
goDrinkLowHealthUnsafe :: Integer -> Strategy Dir
goDrinkLowHealthUnsafe lim st = do
  guard $ myHp st <= lim
  goToTile ((== TavernTile).snd) st

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
                     _                 -> True

-- | Attack killable players within attack range
--   that can also kill me, unless they are camping
--   OR, if they are immediately killable
selfDefence :: Strategy Dir
selfDefence st = do
  [h] <- offensiveHeros (\h -> heroLife h <= attackDamage ||
                             ( isKillable (myHp st) (heroLife h)
                             && isKillable (heroLife h) (myHp st)
                             && not (isCamping (board st) (me st) h) )
                        ) st
  moveTowards (heroPos h) st
  -- TODO: Handle multiple case

-- | Attack killable players within attack range
--   if it is profitable, unless they are near a tavern
profitDefence :: Strategy Dir
profitDefence st = do
  [h] <- offensiveHeros (\h -> isKillable (myHp st) (heroLife h)
                            && heroMineCount h > 0
                            && not (isNearTavern (board st) (heroPos h))
                        ) st
  moveTowards (heroPos h) st

-- | Flee from other players attack range
flee :: Strategy Dir
flee st = do
  hs <- map heroPos <$> offensiveHeros (const True) st
  guard . not $ null hs
  let as = S.fromList $ hs ++ concat (map fst . attackRange (board st) <$> hs)
  goToTile (\(p,t) -> isPassable t && not (p `S.member` as)) st
