{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gauge where

import           Control.Lens
import           Data.IORef
import qualified Prelude
import           Protolude
import           System.Random
import           Test.Hspec

data Gauge a = Gauge
  { _gState   :: IORef (a, StdGen)
  , _gMaxIncr :: a
  , _gMaxVal  :: a }
makeLenses ''Gauge
instance Show a => Show (Gauge a) where
  show (Gauge _ i v) = "Gauge <ioref> i=" <> show i <> " mv=" <> show v

new :: MonadIO m => StdGen -> a -> a -> a -> m (Gauge a)
new stdGen start maxIncr maxVal
  =  Gauge
 <$> liftIO (newIORef (start, stdGen))
 <*> pure maxIncr
 <*> pure maxVal

new137 :: MonadIO m => a -> a -> a -> m (Gauge a)
new137  = new (mkStdGen 137)

readGauge :: (MonadIO m, Num a, Ord a, Random a) => Gauge a -> m a
readGauge g = do
  (v, gen) <- liftIO (readIORef (g^.gState))
  if v >= g^.gMaxVal then pure v
    else do
      let (v', gen') = randomR (v, v + g^.gMaxIncr) gen
      if v' > g^.gMaxVal then pure (g^.gMaxVal)
        else do
          liftIO (writeIORef (g^.gState) (v', gen'))
          pure v'

g1t :: Spec
g1t  = do
  g <- runIO (new137 0.0 1.0 (10.0::Double))
  r <- runIO (forM [1 .. 22::Int] $ const (readGauge g))
  it "Gauge" $ r `shouldBe`
    [ 0.12338499777122891, 1.0708819156961413, 1.277325880958806, 1.8065760996471818, 2.7689496076046365
    , 2.9227298520953804 , 3.2958818137864014, 3.383306251884114, 4.16275469564574  , 4.934069408756031
    , 5.499566955626872  , 6.03605536567001  , 6.309264907078681, 6.64842762532331  , 7.21534188125697
    , 7.751394829861742  , 7.918681232228057 , 8.518857999026073, 9.486094088349343 , 9.664784042000182
    , 10.0               , 10.0]

------------------------------------------------------------------------------

data GaugeException = ExceedsMaxFlow | NotDecr deriving (Eq, Show)
instance Exception GaugeException

checkReadings
  :: Num a
  => (a -> Bool) -> GaugeException -> [a]
  -> Either GaugeException [a]
checkReadings p e xs = if all p (elemDiff xs) then Right xs else Left e

elemDiff :: Num a => [a] -> [a]
elemDiff xs = zipWith (-) xs (Prelude.tail xs)

-- expects input to be in DESCREASING order
-- exception if difference between any consecutive elements exceeds given 'maxFlow'
checkMaxFlow :: (Num a, Ord a) => a -> [a] -> Either GaugeException [a]
checkMaxFlow maxFlow = checkReadings (<= maxFlow) ExceedsMaxFlow

-- expects input to be in DESCREASING order
-- exception if not
checkDecr :: (Num a, Ord a) => a -> [a] -> Either GaugeException [a]
checkDecr          z = checkReadings       (>= z) NotDecr

gc1t :: Spec
gc1t  = do
  let maxFlow = 1.0
  g1 <- runIO (new137 0.0 maxFlow (10.0::Double))
  g2 <- runIO (new137 0.0     3.0 (10.0::Double))
  r1 <- runIO (forM [1 .. 22::Int] $ const (readGauge g1))
  r2 <- runIO (forM [1 .. 22::Int] $ const (readGauge g2))
  let r1r = reverse r1
  let r2r = reverse r2
  it "checkMaxFlow OK"       $ checkMaxFlow maxFlow r1r `shouldBe` Right r1r
  it "checkMaxFlow except 1" $ checkMaxFlow     0.5 r1r `shouldBe` Left ExceedsMaxFlow
  it "checkMaxFlow except 2" $ checkMaxFlow maxFlow r2r `shouldBe` Left ExceedsMaxFlow
  it "checkDecr OK"          $ checkDecr        0.0 r1r `shouldBe` Right r1r
  it "checkMaxFlow except 1" $ checkDecr        0.0 r1  `shouldBe` Left NotDecr
  it "checkMaxFlow except 2" $ checkDecr        0.0 r2  `shouldBe` Left NotDecr
