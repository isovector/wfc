{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Control.Comonad
import Control.Comonad.Store
import Control.Lens
import Data.Coerce
import qualified Data.Vector as V
import Codec.Picture
import BasePrelude
import Linear.V2
import Linear.Matrix

bricks :: Image PixelRGB8
bricks =
  case fromRight undefined $ unsafePerformIO $ readImage "3Bricks.png" of
     ImageY8    _   -> error "ImageY8"
     ImageY16   _   -> error "ImageY16"
     ImageYF    _   -> error "ImageYF"
     ImageYA8   _   -> error "ImageYA8"
     ImageYA16  _   -> error "ImageYA16"
     ImageRGB8  img -> img
     ImageRGB16 _   -> error "ImageRGB16"
     ImageRGBF  _   -> error "ImageRGBF"
     ImageRGBA8 _   -> error "ImageRGBA8"
     ImageRGBA16 _  -> error "ImageRGBA16"
     ImageYCbCr8 _  -> error "ImageYCbCr8"
     ImageCMYK8  _  -> error "ImageCMYK8"
     ImageCMYK16  _ -> error "ImageCMYK16"

newtype Pattern a = Pattern
  { getPattern :: V.Vector a
  }
  deriving (Eq, Ord, Show)

type Coord = V2 Int

rotMat :: M22 Int
rotMat = V2 (V2 0 (-1)) (V2 1 0)

refMat :: M22 Int
refMat = V2 (V2 (-1) 0) (V2 0 (-1))

refRotMat :: M22 Int
refRotMat = refMat !*! rotMat


packPattern :: Pixel a => Int -> Coord -> Image a -> Pattern a
packPattern n (V2 x y) img = Pattern . V.fromList $ do
  y <- [y..y+n-1]
  x <- [x..x+n-1]
  pure $ pixelAt img x y

packPosition :: Int -> Coord -> Int
packPosition n (V2 x y) = y * n + x


frequency :: Ord a => [a] -> [(a, Int)]
frequency = fmap (head &&& length) . groupBy (==) . sort


wrap :: Int  -- ^ v
     -> Int  -- ^ max
     -> (Int, Int)
wrap x n
 | x < 0 = (0, x + n)
 | otherwise = (x, n)


agrees
    :: Eq a
    => Int
    -> Pattern a
    -> Pattern a
    -> Coord
    -> Bool
agrees n (Pattern p1) (Pattern p2) (V2 dx dy) = and $ do
  x <- uncurry enumFromTo $ fmap (subtract 1) $ wrap dx n
  y <- uncurry enumFromTo $ fmap (subtract 1) $ wrap dy n
  pure $ p1 V.! packPosition n (V2 x y)
      == p2 V.! packPosition n (V2 (x - dx) (y - dy))


allPatterns
    :: Pixel a
    => Int   -- ^ n
    -> Image a
    -> [Pattern a]
allPatterns n img = do
  x <- [0..imageWidth img]
  y <- [0..imageHeight img]
  guard $ x + n < imageWidth img
  guard $ y + n < imageHeight img
  let xy = V2 x y
  coerce $ fmap (\c -> packPattern n (c !* xy) img)
    -- TODO(sandy): this transforms should be in local space, not img space
    [ identity
    -- , refMat
    -- , rotMat
    -- , refMat !*! rotMat
    -- , rotMat !*! rotMat
    -- , refMat !*! rotMat !*! rotMat
    -- , rotMat !*! rotMat !*! rotMat
    -- , refMat !*! rotMat !*! rotMat !*! rotMat
    ]

type Quantum = Store Coord

observe :: Int -> Pattern a -> Quantum a -> Quantum (Maybe a)
observe n p w =
  let (V2 x y) = pos w
   in undefined

