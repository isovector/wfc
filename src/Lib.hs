{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-imports -ddump-splices #-}

module Lib where

import           BasePrelude hiding (peek, rotate)
import           Codec.Picture
import           Control.Comonad
import           Control.Comonad.Representable.Store
import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           Data.Distributive
import           Data.Functor.Compose (Compose (..))
import           Data.Functor.Rep
import           Data.Generics.Sum
import qualified Data.Set as S
import qualified Data.Vector as V
import           GHC.TypeLits
import           Language.Haskell.Codo
import           Linear.Matrix hiding (trace)
import           Linear.V2

deriving instance Generic PixelRGB8
deriving instance NFData PixelRGB8

data SuperPos a
  = SuperPos (S.Set a)
  | Collapsed a
  deriving Generic

superposit :: (S.Set a -> S.Set a) -> SuperPos a -> SuperPos a
superposit _ z@(Collapsed _) = z
superposit f (SuperPos s)    = SuperPos $ f s

superpose :: Ord b => (a -> b) -> SuperPos a -> SuperPos b
superpose f (Collapsed z) = Collapsed $ f z
superpose f (SuperPos s)  = SuperPos $ S.map f s


newtype Grid (width :: Nat) (height :: Nat) a = Grid
    { getGrid :: Compose V.Vector V.Vector a
    }
  deriving (Functor, Show, Foldable, Eq, Traversable)

instance (KnownNat m, KnownNat n) => Distributive (Grid m n) where
  distribute = distributeRep

instance (KnownNat m, KnownNat n) => Representable (Grid m n) where
  type Rep (Grid m n) = Coord
  index (Grid (Compose v)) (V2 x y) = v V.! x V.! y
  tabulate f = Grid . Compose $ V.generate (fromIntegral $ natVal $ Proxy @m) go
    where
      go x = V.generate (fromIntegral $ natVal $ Proxy @n) (\y -> f (V2 x y))



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


packPattern :: Pixel a => Int -> (Coord -> Coord) -> Coord -> Image a -> Maybe (Pattern a)
packPattern n f xy img = fmap (Pattern . V.fromList) . sequenceA $ do
  y' <- [0..n-1]
  x' <- [0..n-1]
  let dxy = f $ V2 x' y'
  pure $ samplePixel img $ xy + dxy

samplePixel :: Pixel a => Image a -> Coord -> Maybe a
samplePixel img (V2 x y) = do
  guard $ x >= 0 && x < imageWidth img
  guard $ y >= 0 && y < imageHeight img
  pure $ pixelAt img x y

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
 -- | x > n = wrap (x - n) n
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


allPatterns
    :: (Pixel a, NFData a)
    => Int   -- ^ n
    -> Image a
    -> [Pattern a]
allPatterns n img = force $ do
  y <- [0..imageHeight img-1]
  x <- [0..imageWidth img-1]
  let xy = V2 x y
  fromMaybe [] . sequenceA $ fmap (\c -> packPattern n c xy img)
    -- TODO(sandy): this transforms should be in local space, not img space
    [ id
    , reflect n
    , rotate n
    , reflect n . rotate n
    , rotate n  . rotate n
    , reflect n . rotate n . rotate n
    , rotate n  . rotate n . rotate n
    , reflect n . rotate n . rotate n . rotate n
    -- , rotMat
    -- , refMat !*! rotMat
    -- , rotMat !*! rotMat
    -- , refMat !*! rotMat !*! rotMat
    -- , rotMat !*! rotMat !*! rotMat
    -- , refMat !*! rotMat !*! rotMat !*! rotMat
    ]

rotMat :: Num a => M22 a
rotMat = V2 (V2 0 (-1))
            (V2 1 0)

reflect :: Int -> Coord -> Coord
reflect n = _x %~ \x -> n - 1 - x

rotate :: Int -> Coord -> Coord
rotate (subtract 1 -> fromIntegral @_ @Float -> (/2) -> n) (fmap fromIntegral -> xy) =
  fmap floor $ (+ V2 n n) $ rotMat !* (xy - V2 n n)

type WIDTH = 32
type HEIGHT = 32
type Quantum = Store (Grid WIDTH HEIGHT)

initialize
    :: (Pixel a, Ord a, NFData a)
    => Int
    -> Image a
    -> Quantum (SuperPos (Int, Pattern a))
initialize n img = store (const . SuperPos . S.fromList . frequency $ allPatterns n img) (V2 0 0)


observe
    :: (Ord a, Show a)
    => Int  -- ^ n
    -> Pattern a
    -> Coord
    -> Quantum (SuperPos (Int, Pattern a))
    -> SuperPos (Int, Pattern a)
observe n p xy w =
  stamp n xy p w


stamp
    :: (Show a, Eq a)
    => Int
    -> Coord
    -> Pattern a
    -> Quantum (SuperPos (Int, Pattern a))
    -> SuperPos (Int, Pattern a)
stamp n xy p w =
  let dxy@(V2 dx dy) = pos w - xy
      q = extract w
   in case q of
        SuperPos ps ->
          case (xy == pos w, -n + 1 <= dx && dx < n  && -n + 1 <= dy && dy < n) of
            (True, _) -> Collapsed (0, p)
            (_, True) -> SuperPos
                  . S.filter (\(snd -> p') -> agrees n dxy p' p)
                  $ ps
            (_, False) -> q
        Collapsed z -> Collapsed z


neighbors :: Coord -> [Coord]
neighbors (V2 x y) = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard $ not $ dx == 0 && dy == 0
  pure $ V2 (x + dx) (y + dy)


isCollapsing :: SuperPos a -> Maybe a
isCollapsing (Collapsed _) = Nothing
isCollapsing (SuperPos ps) = bool Nothing (Just $ head $ toList ps) $ length ps == 1


collapse
    :: Eq a
    => Int
    -> Quantum (SuperPos (Pattern a))
    -> SuperPos (Pattern a)
collapse n w =
  case extract w of
    z@Collapsed{}  -> z
    x@(SuperPos a) ->
      case isCollapsing x of
        Just z  -> Collapsed z
        Nothing ->
          let ns   = experiment neighbors $ w =>> pos &&& extract
              rels = mapMaybe (traverse isCollapsing) ns
           in SuperPos $ flip S.filter a $ \p' ->
                flip all rels $ \(xy', p) ->
                  agrees n (xy - xy') p p'
  where
    xy = pos w


-- collapse = [codo| w =>
--     p   <- pos w
--     ns  <- experiment neighbors w
--     ns' <- (pos &&& extract) ns
--     let rels = traverse _ $ extract ns'
--     ns
--   |]






entropy :: Quantum (SuperPos a) -> Int
entropy w =
  case extract w of
    SuperPos z  -> S.size z
    Collapsed _ -> 0


getStore :: Store w a -> w a
getStore (StoreT w _) = runIdentity w


minEntropy :: Quantum (SuperPos a) -> (Coord, Int)
minEntropy q
  = minimumBy (comparing snd)
  . filter ((> 0) . snd)
  . toList
  . getStore
  $ q =>> pos &&& entropy


patternToPixel :: Pattern a -> a
patternToPixel (Pattern v) = V.head v


blend :: SuperPos PixelRGB8 -> PixelRGB8
blend (Collapsed ps) = ps
blend (SuperPos ps)
  | S.null ps = PixelRGB8 255 0 255
  -- | S.size ps == 1 =
  --     let ZipList [r, g, b] = unpack $ head $ toList ps
  --      in (PixelRGB8 255 255 0)
  | otherwise =
      let elems = fmap unpack $ toList ps
          len = length elems
          ZipList [r', g', b'] =
            fmap (fromIntegral . (`div` len) . sum . fmap fromIntegral) $ sequenceA elems
      in PixelRGB8 r' g' b'
  where
    unpack (PixelRGB8 r g b) = ZipList [r, g, b]


getSize :: forall m n a. (KnownNat m, KnownNat n) => Store (Grid m n) a -> (Int, Int)
getSize _ = ( fromIntegral . natVal $ Proxy @m
            , fromIntegral . natVal $ Proxy @n
            )


render :: Quantum (SuperPos PixelRGB8) -> Image PixelRGB8
render q = uncurry (generateImage makePixel) $ getSize q
  where
    makePixel x y = peek (V2 x y) $ fmap blend q


twist :: Int -> Coord -> Integer
twist a (V2 x y) = (fromIntegral x ^ (5 :: Int) + fromIntegral y ^ (3 :: Int))
  `div` fromIntegral a


weighted :: Show a => Int -> Coord -> SuperPos (Int, a) -> a
weighted _ _ (Collapsed (_, s)) = s
weighted a xy (SuperPos (S.toList -> s)) =
  let size = sum $ fmap fst s
      total = join $ fmap (uncurry replicate) s
   in total !! (fromIntegral (twist a xy `mod` fromIntegral size))


main :: IO ()
main = do
  let n = 3
      q'' = iterate
              (\(a, q) ->
                (a+1,) $
                  let epos = fst $ minEntropy q
                      collapsed = weighted a epos $ peek epos q
                   in q =>> observe n collapsed epos)
          $ (2, initialize n bricks)
  for_ [799..800] $ \i -> do
    let q' = snd $ q'' !! i
    writePng "result.png"
      . render
      $ fmap (superpose (V.head . getPattern) . superpose snd) q'
    threadDelay 250000
  pure ()

