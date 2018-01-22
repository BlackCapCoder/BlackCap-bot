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
  [ murderWeak
  , selfDefence
  , profitDefence
  , goDrinkLowHealth attackDamage
  -- , goDrinkLowHealthUnsafe attackDamage
  , drinkNextToTavern 70
  , flee
  , stealMine
  , goDrinkLowHealth 80
  ]


--------------------
----- Strategy -----
--------------------

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


----------------------------
---- Utility strategies ----
----------------------------

-- | Moves the player towards a given point
moveTowards :: Pos -> Strategy Dir
moveTowards p st = do
  (h:_) <- pathTo' (board st) (isPassable.snd) (myPos st) p
  return h

-- | Go to the closest tile matching a description
goToTile :: ((Pos, Tile) -> Bool) -> Strategy Dir
goToTile f = goToTile' f (isPassable.snd)

-- | Makes sure not to move within the attack range of a non-killable player
safeGoToTile :: ((Pos, Tile) -> Bool) -> Strategy Dir
safeGoToTile f st
  = goToTile' f (\x@(_,t) -> isPassable t && f' x) st
  where f' (p, t) = not $ null [ h | h <- offensiveHeroes (game st) p
                               , heroId h /= myId st
                               , isKillable (heroLife h) (myHp st) ]
    -- TODO: A tavern could save a path

-- | Go to the closest tile matching some description
goToTile'
  :: ((Pos, Tile) -> Bool) -- ^ Target tiles
  -> ((Pos, Tile) -> Bool) -- ^ Walkable tiles
  -> Strategy Dir
goToTile' f1 f2 st = do
  (h:_) <- closestPath' (board st) f2 (myPos st) . map fst . findTiles f1 $ board st
  return h


-- | Get to the nearest tavern as quickly as possible and get a drink!
goDrink :: Strategy Dir
goDrink = safeGoToTile ((== TavernTile).snd)

-- | Goes to the closest mine satisfying a condition
goMine :: (Tile -> Bool) -> Strategy Dir
goMine f = safeGoToTile (\(_,t) -> isTileMine t && f t)

-- | Go to the closest hero satisfying a condition, that isn't yourself
goHero :: (Tile -> Bool) -> Strategy Dir
goHero f st = safeGoToTile (\(_,t) -> isTileHero t && (\(HeroTile i) -> i /= myId st) t && f t) st


---------------------
---- Strategies -----
---------------------

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

-- | Execute weak players in attack range
murderWeak :: Strategy Dir
murderWeak st = do
  (h:_) <- pure [ x | x <- offensiveHeroes (game st) (myPos st)
                , heroLife x <= attackDamage ]
  moveTowards (heroPos h) st

-- | Attack killable players within attack range
--   that can also kill me, unless they are camping
selfDefence :: Strategy Dir
selfDefence st = do
  (h:_) <- pure [ x | x <- offensiveHeroes (game st) (myPos st)
                , isKillable (myHp st   ) (heroLife x)
                , isKillable (heroLife x) (myHp st   )
                , not $ isCamping (board st) (me st) x
                ]
  moveTowards (heroPos h) st
  -- TODO: Handle multiple case

-- | Attack killable players within attack range
--   if it is profitable, unless they are near a tavern
profitDefence :: Strategy Dir
profitDefence st = do
  (h:_) <- pure [ x | x <- offensiveHeroes (game st) (myPos st)
                , isKillable (myHp st) (heroLife x)
                , heroMineCount x > 0
                , not $ isNearTavern (board st) (heroPos x)
                ]
  moveTowards (heroPos h) st
  -- TODO: Handle multiple case

-- | Flee from other players attack range
flee :: Strategy Dir
flee st = do
  let hs = heroPos <$> offensiveHeroes (game st) (myPos st)
  guard . not $ null hs
  let as = S.fromList $ hs ++ concat (map fst . attackRange (board st) <$> hs)
  goToTile (\(p,t) -> isPassable t && not (p `S.member` as)) st
