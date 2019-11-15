{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Online.Random where

import Prelude as P
import Online
import Control.Monad.Primitive (PrimState, PrimMonad)
import Streaming.Internal
import Streaming.Prelude as S
import System.Random.MWC
import System.Random.MWC.Probability
import qualified Control.Foldl as L
import qualified Data.Sequence as Seq
import qualified Streaming as S
import Control.Lens hiding ((:>), each)
import Data.List ((!!))
import Data.Maybe
import GHC.Generics

nearZero :: (Ord a, Fractional a) => a -> Bool
nearZero a = a < 1e-6 && a > -1e-6

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Streaming.Prelude as S
-- >>> gen <- create
-- >>> let n = 3
-- >>> let eq' a b = P.all nearZero $ P.zipWith (-) a b
-- >>> let eq'p a b = P.all id $ P.zipWith (\(x0,x1) (y0,y1) -> nearZero (x0-y0) P.&& nearZero (x1-y1)) a b
--

-- | rvs creates a list of standard normal random variates.
-- >>> t <- rvs gen n
-- >>> t `eq'` [-0.8077385934202513,-1.3423948150518445,-0.4900206084002882]
-- True
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standardNormal gen

-- | rvs_ is a standard random variate stream
-- >>> t <- rvs_ gen & S.take n & S.toList_
-- >>> t `eq'` [-0.8077385934202513,-1.3423948150518445,-0.4900206084002882]
-- True
--
-- stack build --ghc-options=-fsimpl-tick-factor=1000
-- rvs_ :: Gen (PrimState IO) -> S.Stream (S.Of Double) IO ()
rvs_ :: PrimMonad m => Gen (PrimState m) -> S.Stream (S.Of Double) m ()
rvs_ gen = repeatM (sample standardNormal gen)

-- | rvsPair generates a list of correlated random variate tuples
-- | 
-- >>> t <- rvsp gen 3 0.8
-- >>> t `eq'p` [(-0.8077385934202513,-1.4591410449385904),(-1.3423948150518445,-0.6046212701237168),(-0.4900206084002882,0.923007518547542)]
-- True
rvsp :: Gen (PrimState IO) -> Int -> Double -> IO [(Double,Double)]
rvsp gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = P.zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ P.zip s0 s1'

-- | rvsp_ is a stream of correlated rv pairs where the correlation is also a stream
-- >>> t <- rvsp_ gen (S.repeat 0.8) & S.take 3 & S.toList_
-- >>> t `eq'p` [(-0.8077385934202513,-1.4516277637673076),(-0.4900206084002882,-1.2049666569226198),(0.7821576365295985,1.9407501144914514)]
-- True
rvsp_ :: Gen (PrimState IO) -> S.Stream (S.Of Double) IO () -> S.Stream (S.Of (Double, Double)) IO ()
rvsp_ gen =
    S.zipWith3 (\x y c' -> (x, c' * x + sqrt (1 - c' * c') * y)) (rvs_ gen) (rvs_ gen)

-- | rv_ is a normally distributed stream where mean and sd are supplied by other streams
-- >>> t <- rv_ gen (S.repeat 10) (S.repeat 0.1) & S.take n & S.toList_
-- >>> t `eq'` [9.919226140657974,9.865760518494815,9.95099793915997]
-- True
rv_ :: PrimMonad m => Gen (PrimState m) -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m ()
rv_ gen m s = S.zipWith3 (\m' s' r -> m' + s' * r) m s (repeatM (sample standardNormal gen))

rvStd :: L.PrimMonad m => Gen (PrimState m) -> Stream (Of Double) m ()
rvStd gen = rv_ gen (S.repeat 0) (S.repeat 1)

stdReg :: L.PrimMonad m => Gen (PrimState m) -> Stream (Of (Double, Double)) m ()
stdReg gen = xy_ (S.repeat 0) (S.repeat 1) (rvStd gen) (rvStd gen)

-- | y_ is the proto-typical linear relationship
-- y = b x + a + e
-- where all the bits are streams
-- implicitly they are all independent as well
-- >>> t <- y_ (S.repeat 1) (S.repeat 1) (rv_ gen (S.repeat 1) (S.repeat 1)) (rv_ gen (S.repeat 0) (S.repeat 1)) & S.take n & S.toList_
-- >>> t `eq'` [-0.1501334084720959,0.155062441262396,4.973864311975887]
-- True
y_ :: PrimMonad m => S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m ()
y_ a b e x = S.zipWith (+) e $ S.zipWith3 (\a' b' x' -> b' * x' + a') a b x

-- | xy_ returns a (x,y) regressed pair stream
-- >>> t <- xy_ (S.repeat 0) (S.repeat 1) (rv_ gen (S.repeat 0) (S.repeat 1)) (rv_ gen (S.repeat 0) (S.repeat 1)) & S.take n & S.toList_
-- >>> t `eq'p` [(-0.8077385934202513,-2.150133408472096),(-0.4900206084002882,-1.844937558737604),(0.7821576365295985,2.9738643119758867)]
-- True
xy_ :: PrimMonad m => S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of Double) m () -> S.Stream (S.Of (Double, Double)) m ()
xy_ a b e x = S.zipWith (\(x',y) e' -> (x',y+e')) (S.zipWith3 (\a' b' x' -> (x',b' * x' + a')) a b x) e

-- | traverse the same fold over a list
foldMapL :: L.Fold a b -> L.Fold [a] [b]
foldMapL (L.Fold step begin extract) = L.Fold fstep fbegin fextract
  where
    fbegin = Nothing
    fstep Nothing as = Just $ step begin <$> as
    fstep (Just xs) as = Just $ P.zipWith step xs as
    fextract Nothing = []
    fextract (Just xs) = extract <$> xs

-- | a constant L.Fold
fconst :: b -> L.Fold a b
fconst a = L.Fold (\ _ _ -> ()) () (P.const a)

-- | apply a list of Folds to a Foldable
foldList :: [L.Fold a b] -> L.Fold a [b]
foldList = P.foldr (\f -> (<*>) ((:) <$> f)) (fconst [])
-- foldList [] = fconst []
-- foldList (f:fs) = (:) <$> f <*> foldList fs

-- | turn a stream list into a list of streams
streamList :: [Stream (Of Double) IO ()] -> Stream (Of [Double]) IO ()
streamList = P.foldr (S.zipWith (:)) (yield [])
-- streamList [] = yield []
-- streamList (f:fs) = zipWith (:) f (streamList fs)


-- | streams are tricky to delay and zip when the logic gets loopy
-- eg. just delaying a stream via `yield P.nan >> s` and `copy . _1 . unseparate` doesn't work
-- delaying a fold is a lot easier
-- delay_ is a fold that delays the Foldable by n steps
delay_ :: Int -> L.Fold a (Maybe a)
delay_ n = L.Fold (\x a -> Seq.drop 1 $ x Seq.|> Just a) (Seq.fromList $ P.replicate n Nothing) ((\case { Seq.EmptyL -> Nothing; (x Seq.:< _) -> x}) . Seq.viewl)

-- | `delay nan 1` delays a Foldable by 1 step, substituting nan at the beginning
-- note that L.scan places the initial accumulator value at the start of the resulting Foldable/Streamable eg.
-- > L.scan (delay' P.nan 1) [0..3]
-- [NaN,NaN,0.0,1.0,2.0]
--
delay' :: a -> Int -> L.Fold a a
delay' begin n = fmap (fromMaybe begin) (delay_ (n+1))

-- | transform a stream into a tuple where the first element is the ori=ginal stream and the second element is the delayed original stream
echo :: (P.Num a) =>
    Stream (Of a) IO () ->
    Stream (Of (a,a)) IO ()
echo rv' = rv' & L.purely scan ((,) <$> delay' 0 0 <*> delay' 0 1) & S.drop 1

second :: (P.Num a) => L.Fold b c -> Stream (Of (a,b)) IO () -> Stream (Of (a,c)) IO ()
second f s = s & L.purely scan ((,) <$> L.handles _1 (delay' 0 0) <*> L.handles _2 f) & S.drop 1

-- | branching pipe
-- each [1..4] & branch (drop 1 & L.purely scan L.sum) (map show) & eitherToPair & toList_
branch :: (Functor f, Monad (t (Stream f m2)), Monad m1, Monad m2, Monad m, S.MFunctor t, S.MonadTrans t) => (t (Stream f m2) r2 -> Stream (Of a1) (Stream (Of b) m1) r1) -> (Stream (Of a) (Stream (Of a) m) r -> Stream f (t m2) r2) -> Stream (Of a) m r -> Stream (Of (Either a1 b)) m1 r1
branch m0 m1 s = s & copy & m1 & distribute & m0 & unseparate & maps sumToEither

-- | turn a stream of `Either a a` into (a,a)
eitherToPair :: (Monad m) => Stream (Of (Either Double Double)) m r -> Stream (Of (Double,Double)) m r
eitherToPair = loop Nothing Nothing where
  loop stateL stateR str = case str of
    Return r -> pure r
    Effect m -> Effect (fmap (loop stateL stateR) m)
    Step (Left l :> rest) -> case stateR of
      Nothing -> loop (Just l) Nothing rest
      Just r' -> do
        yield (l,r')
        loop Nothing Nothing rest
    Step (Right r :> rest) -> case stateL of
      Nothing -> loop Nothing (Just r) rest
      Just l' -> do
        yield (l',r)
        loop Nothing Nothing rest

-- | double a stream with 2 transformations
-- > toList_ $ doubler (map identity) (map identity) (each [0..3])
-- [0,0,1,1,2,2,3,3]
--
doubler :: (Functor f, Monad (t (Stream f m2)), Monad m1, Monad m2, Monad m, S.MFunctor t, S.MonadTrans t) => (t (Stream f m2) r2 -> Stream (Of a1) (Stream (Of a1) m1) r1) -> (Stream (Of a) (Stream (Of a) m) r -> Stream f (t m2) r2) -> Stream (Of a) m r -> Stream (Of a1) m1 r1
doubler m0 m1 s = s & copy & m1 & distribute & m0 & unseparate & maps unify
  where
    unify :: S.Sum (Of a) (Of a) m -> Of a m
    unify (S.InL a) = a
    unify (S.InR a) = a

-- | like scan, but use the first value as the starting point
-- > scan0 (\_ a -> a) id id == id
--
-- not sure if ...
-- > L.purely scan0 == drop1 . L.purely scan
--
scan0 :: Monad m => (x -> a -> x) -> (a -> x) -> (x -> b) -> Stream (Of a) m r -> Stream (Of b) m r
scan0 acc beginf done s = do
    n <- S.lift $ next s
    case n of
      Left r -> pure r
      (Right (a, rest)) ->
          scan acc (beginf a) done rest

-- | the classical abstract historio-dependent model of a stream
data DepMoments a = DepMoments
    { betas :: Stream (Of [a]) IO ()
    , xxs :: [L.Fold a a]
    , rv :: Stream (Of a) IO ()
    , fx :: [(Int, Int)]
    , fstd :: [(Int, Int)]
    } deriving (Generic)

-- defModel :: [Double] -> IO (DepMoments Double)
defModel :: Gen L.RealWorld -> [Double] -> DepMoments Double
defModel g bs = DepMoments (S.repeat bs) [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9] (rvStd g) (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5])

-- | create a stream from a DepMoments model
-- >>> toList_ $ depMo (DepMoments (S.repeat [0,0,0,1,0,0]) [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9] (each [0..5]) (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5]))
-- [0.0,1.0,2.0,3.0,4.0,5.0]
--
-- mean momentum
-- >>> toList_ $ depMo (DepMoments (S.repeat [0,1,0,1,0,0]) [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9] (each [0..5]) (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5]))
-- [0.0,1.0,2.3690036900369003,3.8432683919744113,5.369929916241361,6.931240249360273]
--
depMo ::
    DepMoments Double ->
    Stream (Of Double) IO ()
depMo d =
    rv d &
    echo &
    second (foldList (xxs d)) &
    S.zipWith (\bs (rv0,xs) ->
               L.fold L.sum (P.fmap (\(bi, xi) -> bs!!bi * xs!!xi) (fx d)) +
               rv0 * L.fold L.sum (P.fmap (\(bi, xi) -> bs!!bi * xs!!xi) (fstd d)))
    (betas d)
