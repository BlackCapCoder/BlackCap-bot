module BotHelper
  where

import Data.Graph.AStar
import Data.Maybe
import Data.List
import Vindinium
import qualified Data.HashSet as H

type Index = Int

-- | Convert a point into an index
pos2Ix :: Board -> Pos -> Index
pos2Ix b (Pos x y) = y * boardSize b + x

-- | Convert an index into a point
ix2Pos :: Board -> Index -> Pos
ix2Pos b ix = Pos (mod ix $ boardSize b) (div ix $ boardSize b)


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

-- | Calculates the taxicab distance between two points
taxicabDist (Pos x1 y1) (Pos x2 y2) = abs (x2-x1) + abs (y2-y1)

-- | Returns neighboring tiles around a point
neighbors :: Board -> Pos -> [(Pos, Tile)]
neighbors b c
  = map fromJust
  . filter isJust
  . zipWith (\p m -> (,) p <$> m) ps
  $ map (tileAt b) ps
  where ps = map (c+) $ zipWith Pos <*> reverse $ [0,0,-1,1]

-- | Finds the shortest path between two points, does not include starting point
pathTo
  :: Board       -- | The game board
  -> (Tile -> Bool) -- | A function that returns true for non-solid tiles
  -> Pos         -- | Starting position
  -> Pos         -- | Target position
  -> Maybe [Pos] -- | Path to target
pathTo b f from to
  = aStar (H.fromList . map fst . filter (\(p, t) -> p `elem` [from, to] || f t) . neighbors b)
          (\_ _ -> 1)
          (taxicabDist to)
          (==to)
          from

-- | Converts a path into turtle instructions
turtlePath :: [Pos] -> [Dir]
turtlePath (p:ps) = snd $ mapAccumL f p ps
  where f p1@(Pos x1 y1) p2@(Pos x2 y2)
          = (,) p2 $ case () of
                       () | y1 > y2 -> North
                          | x1 < x2 -> East
                          | y1 < y2 -> South
                          | x1 > x2 -> West
                          | otherwise -> Stay

-- | Converts a starting point and a path into turtle instructions
turtlePath' :: Pos -> [Pos] -> [Dir]
turtlePath' p ps = turtlePath $ p:ps

