{-# LANGUAGE LambdaCase #-}
module BotHelper
  where

import Data.Graph.AStar
import Data.Maybe
import Data.List
import Data.List.Split
import Vindinium
import qualified Data.HashSet as H
import PlaneZipper
import qualified Data.List.Zipper as LZ

attackDamage = 20
tavarnHeal   = 50

------------------

type Index = Int

-- | Convert a point into an index
pos2Ix :: Board -> Pos -> Index
pos2Ix b (Pos x y) = y * boardSize b + x

-- | Convert an index into a point
ix2Pos :: Board -> Index -> Pos
ix2Pos b ix = Pos (mod ix $ boardSize b) (div ix $ boardSize b)

-- | Calculates the taxicab distance between two points
taxicabDist :: Pos -> Pos -> Int
taxicabDist (Pos x1 y1) (Pos x2 y2) = abs (x2-x1) + abs (y2-y1)

------------------

-- | Determines wether a point is inside the bounds of the board
inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

-- | Returns a tile given a position
tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! pos2Ix b p
        else Nothing

-- | Neighboring tiles around a point
neighbors :: Board -> Pos -> [(Pos, Tile)]
neighbors b c
  = catMaybes
  . zipWith (\p m -> (,) p <$> m) ps
  $ map (tileAt b) ps
  where ps = map (c+) $ zipWith Pos <*> reverse $ [0,0,-1,1]


{- Attack range:
         #
        ###
       ##@##
        ###
         #       -}
-- | Things that can be attacked in the same turn
attackRange :: Board -> Pos -> [(Pos, Tile)]
attackRange b c
  = catMaybes
  . zipWith (\p m -> (,) p <$> m) ps
  $ map (tileAt b) ps
  where ps = map ((c+).uncurry Pos)
          [                    (0, -2)
          ,          (-1, -1), (0, -1), (1, -1)
          , (-2, 0), (-1,  0),          (1,  0), (2, 0)
          ,          (-1, 1),  (0,  1), (1,  1)
          ,                    (0,  2)
          ]


-- | All tiles and their positions
tiles :: Board -> [(Pos, Tile)]
tiles b
  | s <- boardSize b
  = [ (Pos x y, fromJust p)
    | y <- [0..s-1]
    , x <- [0..s-1]
    , p <- [tileAt b $ Pos x y]
    , isJust p ]

-- | Search for some specific tiles
findTiles :: ((Pos, Tile) -> Bool) -> Board -> [(Pos, Tile)]
findTiles f = filter f . tiles

heroById :: Game -> HeroId -> Maybe Hero
heroById g i = find (\h -> heroId h == i) $ gameHeroes g

isTileHero :: Tile -> Bool
isTileHero (HeroTile _) = True
isTileHero           _  = False

isTileMine :: Tile -> Bool
isTileMine (MineTile _) = True
isTileMine           _  = False

heroTileId (HeroTile i) = i

-----------------

game  = stateGame
me    = stateHero
myPos = heroPos   . me
myId  = heroId    . me
myHp  = heroLife  . me
board = gameBoard . game

------------------

-- | Finds the shortest path between two points, does not include starting point
pathTo
  :: Board                 -- ^ The game board
  -> ((Pos, Tile) -> Bool) -- ^ A function that returns true for non-solid tiles
  -> Pos                   -- ^ Starting position
  -> Pos                   -- ^ Target position
  -> Maybe [Pos]           -- ^ Path to target
pathTo b f from to
  = aStar (H.fromList . map fst . filter (\t@(p,_) -> p `elem` [from, to] || f t) . neighbors b)
          (\_ _ -> 1)
          (taxicabDist to)
          (==to)
          from

-- | Converts a path into turtle instructions
turtlePath :: [Pos] -> Path
turtlePath (p:ps) = snd $ mapAccumL f p ps
  where f p1@(Pos x1 y1) p2@(Pos x2 y2)
          = (,) p2 $ case () of
                       () | y1 > y2 -> North
                          | x1 < x2 -> East
                          | y1 < y2 -> South
                          | x1 > x2 -> West
                          | otherwise -> Stay

-- | Converts a starting point and a path into turtle instructions
turtlePath' :: Pos -> [Pos] -> Path
turtlePath' p ps = turtlePath $ p:ps

-- | Path in terms of turtle moves
pathTo' b f from to = turtlePath' from <$> pathTo b f from to

-- | Distance to walk from one point to another
distanceTo :: Board -> ((Pos, Tile) -> Bool) -> Pos -> Pos -> Maybe Int
distanceTo b f s e = length <$> pathTo b f s e

-- | Gives the path to the closest point by the distance you'd have to walk and not actual distance
closestPath :: Board -> ((Pos, Tile) -> Bool) -> Pos -> [Pos] -> Maybe [Pos]
closestPath b f s ps
  | pths <- catMaybes $ pathTo b f s <$> ps
  , not $ null pths
  = Just $ minimumBy (\a b -> length a `compare` length b) pths
  | otherwise = Nothing

-- | Closest path in terms of turtle moves
closestPath' b f from tos = turtlePath' from <$> closestPath b f from tos

------------------

-- | Determines if hero A can kill hero B given that it's A's turn
--   Returns remaining helth of the hero that lives
--   You can kill a hero at full health (100) with as little as 84 yourself
simFight :: Integer -> Integer -> (Bool, Integer)
simFight x y
  | y<=attackDamage = (True, x)
  | (b, q) <- simFight (max (y-attackDamage-1) 1) x
  = (not b, q)

------------------


-- | Turn the map into a 2D zipper
--   This seemed like a good idea before I had to pathfind
makeMap :: State -> Map
makeMap st = move z
  where b  = gameBoard $ stateGame st
        rs = chunksOf (boardSize b) (boardTiles b)
        z  = Z . LZ.fromList $ LZ.fromList <$> rs
        (Pos x y) = heroPos $ stateHero st
        move = foldl (.) id $ replicate x right ++ replicate y down

-- | Move the zipper given directions
turtle :: Path -> Map -> Map
turtle ds = foldl (.) id
          . flip map ds $ \case
              North -> up
              East  -> right
              South -> down
              West  -> left
              Stay  -> id
