{-
various generations of fake data
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Fake.Data where

import Chart hiding (sample)
import NumHask.Prelude

import Control.Monad.Primitive (PrimState, PrimMonad)
import System.Random.MWC
import System.Random.MWC.Probability
-- import qualified Control.Foldl as L
-- import qualified Protolude as P
import qualified Streaming.Prelude as S

-- $setup
-- >>> gen <- create
-- >>> let n = 3

-- | rvs creates a list of standard normal random variates.
-- >>> rvs gen n
-- [-0.8077385934202513,-1.3423948150518445,-0.4900206084002882]
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standard gen

-- | rvs_ is a standard random variate stream
-- >>> rvs_ gen & S.take n & S.toList_
-- [-0.8077385934202513,-1.3423948150518445,-0.4900206084002882]
--
-- stack build --ghc-options=-fsimpl-tick-factor=1000
-- rvs_ :: Gen (PrimState IO) -> S.Stream (S.Of Double) IO ()
rvs_ :: PrimMonad m => Gen (PrimState m) -> S.Stream (S.Of Double) m ()
rvs_ gen = S.repeatM (sample standard gen)

-- | rvsPair generates a list of correlated random variate tuples
-- | 
-- >>> rvsp gen 3 0.8
-- [(-0.8077385934202513,-1.4591410449385904),(-1.3423948150518445,-0.6046212701237168),(-0.4900206084002882,0.923007518547542)]
rvsp :: Gen (PrimState IO) -> Int -> Double -> IO [(Double,Double)]
rvsp gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zip s0 s1'

-- | rvsp_ is a stream of correlated rv pairs where the correlation is also a stream
-- >>> rvsp_ gen (S.repeat 0.8) & S.take 3 & S.toList_
-- [(-0.8077385934202513,-1.4516277637673076),(-0.4900206084002882,-1.2049666569226198),(0.7821576365295985,1.9407501144914514)]
rvsp_ :: Gen (PrimState IO) -> S.Stream (S.Of Double) IO () -> S.Stream (S.Of (Double, Double)) IO ()
rvsp_ gen c =
    S.zipWith3 (\x y c' -> (x, c' * x + sqrt (1 - c' * c') * y)) (rvs_ gen) (rvs_ gen) c

-- | rv_ is a normally distributed stream where mean and sd are supplied by other streams
-- >>> rv_ gen (S.repeat 10) (S.repeat 0.1) & S.take n & S.toList_
-- [9.919226140657974,9.865760518494815,9.95099793915997]
rv_ :: PrimMonad m => Gen (PrimState m) -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m ()
rv_ gen m s = S.zipWith3 (\m' s' r -> m' + s' * r) m s (S.repeatM (sample standard gen))

-- | y_ is the proto-typical linear relationship
-- y = b x + a + e
-- where all the bits are streams
-- implicitly they are all independent as well
-- >>> y_ (S.repeat 1) (S.repeat 1) (rv_ gen (S.repeat 1) (S.repeat 1)) (rv_ gen (S.repeat 0) (S.repeat 1)) & S.take n & S.toList_
-- [-0.1501334084720959,0.155062441262396,4.973864311975887]
y_ :: PrimMonad m => S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m ()
y_ a b e x = S.zipWith (+) e $ S.zipWith3 (\a' b' x' -> b' * x' + a') a b x


-- | xy_ returns an (x,y) regressed pair stream
-- >>> xy_ (S.repeat 0) (S.repeat 1) (rv_ gen (S.repeat 0) (S.repeat 1)) (rv_ gen (S.repeat 0) (S.repeat 1)) & S.take n & S.toList_
-- [(-0.8077385934202513,-2.150133408472096),(-0.4900206084002882,-1.844937558737604),(0.7821576365295985,2.9738643119758867)]
xy_ :: PrimMonad m => S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of (Double, Double)) m ()
xy_ a b e x = S.zipWith (\(x',y) e' -> (x',y+e')) (S.zipWith3 (\a' b' x' -> (x',b' * x' + a')) a b x) e





