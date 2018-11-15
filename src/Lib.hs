{-# LANGUAGE DeriveAnyClass                                      #-}
{-# LANGUAGE DeriveGeneric                                       #-}
{-# LANGUAGE DerivingStrategies                                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                          #-}
{-# LANGUAGE NoImplicitPrelude                                   #-}
{-# LANGUAGE StandaloneDeriving                                  #-}
{-# LANGUAGE TupleSections                                       #-}
{-# LANGUAGE TypeApplications                                    #-}
{-# LANGUAGE ViewPatterns                                        #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib where

import Data.Functor.Rep
import Control.DeepSeq
import Control.Comonad
import Control.Comonad.Store
import Control.Lens
import Data.Coerce
import qualified Data.Vector as V
import Codec.Picture
import BasePrelude hiding (peek)
import Linear.V2
import Linear.Matrix hiding (trace)
import qualified Data.Set as S

deriving instance Generic PixelRGB8
deriving instance NFData PixelRGB8

showTrace :: Show a => a -> a
showTrace = trace =<< show

showsTrace :: Show b => (a -> b) -> a -> a
showsTrace f = trace =<< show . f

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
  deriving newtype (Eq, Ord, Show, NFData)
  deriving Generic

type Coord = V2 Int

rotMat :: M22 Int
rotMat = V2 (V2 0 (-1)) (V2 1 0)

refMat :: M22 Int
refMat = V2 (V2 (-1) 0) (V2 0 (-1))

refRotMat :: M22 Int
refRotMat = refMat !*! rotMat


packPattern :: Pixel a => Int -> Coord -> Image a -> Pattern a
packPattern n (V2 x y) img = Pattern . V.fromList $ do
  y' <- [y..y+n-1]
  x' <- [x..x+n-1]
  pure $ pixelAt img x' y'

packPosition :: Int -> Coord -> Int
packPosition n (V2 x y) = y * n + x

unpackPattern :: Int -> Coord -> Pattern a -> a
unpackPattern n xy (Pattern v) = v V.! packPosition n xy


frequency :: Ord a => [a] -> [(Int, a)]
frequency = fmap (length &&& head) . groupBy (==) . sort


wrap :: Int  -- ^ v
     -> Int  -- ^ max
     -> (Int, Int)
wrap x n
 | x < 0 = (0, x + n)
 | otherwise = (x, n)


agrees
    :: Eq a
    => Int
    -> Coord
    -> Pattern a
    -> Pattern a
    -> Bool
agrees n (V2 dx dy) (Pattern p1) (Pattern p2) = and $ do
  x <- uncurry enumFromTo $ fmap (subtract 1) $ wrap dx n
  y <- uncurry enumFromTo $ fmap (subtract 1) $ wrap dy n
  pure $ p1 V.! packPosition n (V2 x y)
      == p2 V.! packPosition n (V2 (x - dx) (y - dy))


colors
   :: (Pixel a, Ord a, NFData a)
   => Image a
   -> SuperPos a
colors img = force $ S.fromList $ do
  x <- [0..imageWidth img - 1]
  y <- [0..imageHeight img - 1]
  pure $ pixelAt img x y


allPatterns
    :: (Pixel a, NFData a)
    => Int   -- ^ n
    -> Image a
    -> [Pattern a]
allPatterns n img = force $ do
  y <- [0..imageHeight img]
  x <- [0..imageWidth img]
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

type SuperPos = S.Set

initialize
    :: (Pixel a, Ord a, NFData a)
    => Int
    -> Image a
    -> Quantum (SuperPos (Int, Pattern a))
initialize n img = store (const . S.fromList . frequency $ allPatterns n img) (V2 0 0)


inNSquare :: Int -> Coord -> Bool
inNSquare n (V2 x y) = and
  [ x >= 0
  , x < n
  , y >= 0
  , y < n
  ]

observe
    :: Ord a
    => Int  -- ^ n
    -> Int  -- ^ seed
    -> Coord
    -> Quantum (SuperPos (Int, Pattern a))
    -> SuperPos (Int, Pattern a)
observe n a xy w =
  stamp n xy (weighted a xy $ extract $ seek xy w) w


stamp
    :: Eq a
    => Int
    -> Coord
    -> Pattern a
    -> Quantum (SuperPos (Int, Pattern a))
    -> SuperPos (Int, Pattern a)
stamp n xy p w =
  let dxy@(V2 dx dy) = pos w - xy
   in case abs dx < n && abs dy < n of
        True -> S.filter (\(snd -> p') -> agrees n dxy p p')
                  $ extract w
        False -> extract w


entropy :: Quantum (SuperPos a) -> Int
entropy = S.size . extract


minEntropy :: Int -> Int -> Quantum (SuperPos a) -> (Coord, Int)
minEntropy w h q = minimumBy (comparing fst) $ do
  let f = fst . runStore $ q =>> pos &&& entropy
  x <- [0..w-1]
  y <- [0..h-1]
  let xy = V2 x y
  guard $ snd (f xy) > 1
  pure $ f xy


patternToPixel :: Pattern a -> a
patternToPixel (Pattern v) = V.head v


blend :: SuperPos PixelRGB8 -> PixelRGB8
blend ps
  | S.null ps = PixelRGB8 255 0 255
  | otherwise =
      let unpack = \(PixelRGB8 r g b) -> ZipList [r, g, b]
          elems = fmap unpack $ toList ps
          len = length elems
          ZipList [r', g', b'] =
            fmap (fromIntegral . (`div` len) . sum . fmap fromIntegral) $ sequenceA elems
      in PixelRGB8 r' g' b'


render :: Int -> Int -> Quantum (SuperPos PixelRGB8) -> Image PixelRGB8
render w h (extend (blend . extract) -> q) = generateImage makePixel w h
  where
    makePixel x y = peek (V2 x y) q


twist :: Int -> Coord -> Integer
twist a (V2 x y) = (fromIntegral x ^ (5 :: Int) + fromIntegral y ^ (3 :: Int))
  `div` fromIntegral a


weighted :: Int -> Coord -> SuperPos (Int, a) -> a
weighted a xy (S.toList -> s) =
  let size = sum $ fmap fst s
      total = join $ fmap (uncurry replicate) s
   in total !! (fromIntegral (twist a xy `mod` fromIntegral size))


main :: IO ()
main = do
  let w = 32
      h = 32
      n = 3
      q'' = iterate
              (\(a, q) ->
                (a+1,) $
                  q =>>
                    (observe n (showTrace a)
                      $ fst
                    $ showTrace
                      $ minEntropy w h q))
          $ (2, initialize n bricks)
      q' = snd $ q'' !! 20
  writePng "result.png"
    . render w h
    $ extend (S.map patternToPixel . S.map snd . extract) q'
  pure ()

