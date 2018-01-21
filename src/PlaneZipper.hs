module PlaneZipper
  where

import qualified Data.List.Zipper as LZ
import Control.Comonad


genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> LZ.Zipper (z a)
genericMove a b z =
  LZ.Zip (iterate' a z) (z : iterate' b z)

iterate' :: (a -> a) -> a -> [a]
iterate' f = tail . iterate f

---------------

data Z a = Z (LZ.Zipper (LZ.Zipper a))

up :: Z a -> Z a
up (Z z) = Z (LZ.left z)

down :: Z a -> Z a
down (Z z) = Z (LZ.right z)

left :: Z a -> Z a
left (Z z) = Z (LZ.left <$> z)

right :: Z a -> Z a
right (Z z) = Z (LZ.right <$> z)

cursor :: Z a -> a
cursor (Z z) = LZ.cursor $ LZ.cursor z

safeCursor :: Z a -> Maybe a
safeCursor (Z z) = LZ.safeCursor =<< LZ.safeCursor z

replace :: a -> Z a -> Z a
replace x (Z z) =
  Z $ LZ.replace newLine z
    where
      newLine = LZ.replace x oldLine
      oldLine = LZ.cursor z

horizontal :: Z a -> LZ.Zipper (Z a)
horizontal = genericMove left right

vertical :: Z a -> LZ.Zipper (Z a)
vertical = genericMove up down

neighbours :: [Z a -> Z a]
neighbours = [left, right, up, down]

neighbours' :: Z a -> [Z a]
neighbours' x = ($x) <$> neighbours

----------

instance Comonad LZ.Zipper where
  extract   = LZ.extractz   -- LZ.cursor
  duplicate = LZ.duplicatez -- genericMove LZ.left LZ.right
  extend    = LZ.extendz

instance Comonad Z where
  extract     = cursor
  duplicate z = Z $ horizontal <$> vertical z

instance Functor Z where
  fmap f (Z z) = Z $ fmap (fmap f) z
