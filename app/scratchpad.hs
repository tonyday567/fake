-- [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

-- [libraries](https://www.stackage.org/)
-- [protolude](https://www.stackage.org/package/protolude)
-- [optparse-generic](https://www.stackage.org/package/optparse-generic)
-- [mwc-random](https://www.stackage.org/package/mwc-random)
-- [mwc-probability](https://www.stackage.org/package/mwc-probability)
-- [streaming](https://www.stackage.org/package/streaming)
-- [chart-unit](https://www.github.com/tonyday567/chart-unit)
-- [numhask](https://www.github.com/tonyday567/numhask)

import Options.Generic
import Online.Random
import System.Random.MWC
import qualified Control.Foldl as L
import qualified Streaming as S
import Streaming.Prelude hiding (delay)
import Streaming.Internal
import Online
import qualified NumHask.Prelude as P
import NumHask.Prelude ((<*>), (<$>), Maybe(..), IO, (&), ($), (.), (+), (*), div, Double, (>>), Integer, (<>), Functor, Monad, Int, fmap, Either(..), pure, fromMaybe)
import Control.Category (id)
import Chart
import Control.Monad.Primitive (PrimState)
-- import Text.Pretty.Simple (pPrint)
import qualified Data.Sequence as Seq
import Control.Lens hiding (Wrapped, (&), (:>), Unwrapped)

-- [hoogle](https://www.stackage.org/package/hoogle)

data Opts w = Opts
    { streamMax :: w ::: Maybe Integer <?> "typical size of data stream"
    , testCorr :: w ::: Maybe Double <?> "test correlation"
    , rateCorr :: w ::: Maybe Double <?> "corr rate"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

l1d :: [Double] -> Chart b
l1d = withHud def (lineChart [LineOptions 0.002 (ucolor 0.5 0.8 0.5 1)]) . (:[]) . P.zipWith Pair [0..]
 
rvStd :: L.PrimMonad m => Gen (PrimState m) -> Stream (Of Double) m ()
rvStd gen = rv_ gen (repeat 0) (repeat 1)

stdReg :: L.PrimMonad m => Gen (PrimState m) -> Stream (Of (Double, Double)) m ()
stdReg gen = xy_ (repeat 0) (repeat 1) (rvStd gen) (rvStd gen)

delay1 :: L.Fold a (Maybe a)
delay1 = L.Fold (\_ a -> Just a) Nothing id
-- just delaying the stream via `yield P.nan >> s` say doesn't really work, as the copy . _1 . unseparate trick streams the nan first.  Streams are not zippy by ordering when the logic gets loopy

delay_ :: Int -> L.Fold a (Maybe a)
delay_ n = L.Fold (\x a -> Seq.drop 1 $ x Seq.|> Just a) (Seq.fromList $ P.replicate n Nothing) ((\case { Seq.EmptyL -> Nothing; (x Seq.:< _) -> x}) . Seq.viewl)

delay :: (P.BoundedField a) => Int -> L.Fold a a
delay n = fmap (P.fromMaybe P.nan) (delay_ (n+1))

-- | branching pipe
-- each [1..4] & branch (L.purely scan L.sum) (map (*10)) & eitherToPair & toList_
branch :: (Functor f, Monad (t (Stream f m2)), Monad m1, Monad m2, Monad m, S.MFunctor t, S.MonadTrans t) => (t (Stream f m2) r2 -> Stream (Of a1) (Stream (Of b) m1) r1) -> (Stream (Of a) (Stream (Of a) m) r -> Stream f (t m2) r2) -> Stream (Of a) m r -> Stream (Of (Either a1 b)) m1 r1
branch m0 m1 s = s & copy & m1 & distribute & m0 & unseparate & maps sumToEither

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

maPipe :: (Functor f, Monad (t (Stream f m2)), Monad m1, Monad m2, Monad m, S.MFunctor t, S.MonadTrans t) => (t (Stream f m2) r2 -> Stream (Of a1) (Stream (Of a1) m1) r1) -> (Stream (Of a) (Stream (Of a) m) r -> Stream f (t m2) r2) -> Stream (Of a) m r -> Stream (Of a1) m1 r1
maPipe m0 m1 s = s & copy & m1 & distribute & m0 & unseparate & maps unify
  where
    unify :: S.Sum (Of a) (Of a) m -> Of a m
    unify (S.InL a) = a
    unify (S.InR a) = a

-- | scan0 (\_ a -> a) id id == id
scan0 :: Monad m => (x -> a -> x) -> (a -> x) -> (x -> b) -> Stream (Of a) m r -> Stream (Of b) m r
scan0 acc beginf done s = do
    n <- S.lift $ next s
    case n of
      Left r -> pure r
      (Right (a, rest)) ->
          scan acc (beginf a) done rest

-- stdAuto 1 (repeat 1) (each t1) & to
stdAuto ::
    Double ->
    Stream (Of Double) IO () ->
    Stream (Of Double) IO () ->
    Stream (Of Double) IO ()
stdAuto r b x =
    x &
    L.purely scan ((,) <$> delay 0 <*> delay 1) &
    L.purely scan ((,) <$>
                   L.handles _1 (delay 0) <*>
                   L.handles _2 (L.handles (L.filtered (P.not . P.isNaN)) (ma r)) &
                    fmap (\(x',y') -> (x',if P.isNaN y' then 0 else y'))) &
    drop 2 &
    zipWith (\b' (x',y) -> x'+b'*y) b

autoEst :: Gen L.RealWorld -> Int -> Double -> Double -> Double -> IO [Double]
autoEst gen n b rActual rEst =
        stdAuto rActual (repeat b) (rvStd gen ) &
        take n &
        L.purely scan (autocorr (ma rEst) (corr (ma rEst) (std rEst))) &
        drop 3 &
        toList_


scratchRegression :: Int -> Double -> Double -> Double -> IO ()
scratchRegression n r a b = do
    g <- create
    rc <- xy_ (repeat a) (repeat b) (rvStd g) (rvStd g) & take n & L.purely scan ((,) <$> alpha (ma r) <*> beta (ma r)) & drop 2 & toList_
    let a' = P.fst <$> rc
    let b' = P.snd <$> rc
    fileSvg "other/reg.svg" (600,400) $ withHud def (lineChart [LineOptions 0.002 (ucolor 0.5 0.8 0.5 1), LineOptions 0.002 (ucolor 0.8 0.8 0.5 1), LineOptions 0.002 (ucolor 0.5 0.5 0.5 1), LineOptions 0.002 (ucolor 0.5 0.5 0.5 1)]) [P.zipWith Pair [0..] b', P.zipWith Pair [0..] a', P.take n $ P.zipWith Pair [0..] (P.repeat a), P.take n $ P.zipWith Pair [0..] (P.repeat b)]

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "testing fake data"
    let n = P.maybe 10000 P.fromIntegral (streamMax o)
    let c = fromMaybe 0.8 (testCorr o)
    let r = fromMaybe 0.99 (rateCorr o)
    gen <- create
    pure ()

    avCorr <-
            rvsp_ gen (repeat 0 & take (n `div` 2) >> repeat c) &
            L.purely scan (corr (ma r) (std r)) &
            drop 2 &
            drop 100 &
            take n &
            L.purely fold_ av

    corrL <-
            rvsp_ gen (repeat 0 & take (n `div` 2) >> repeat c) &
            L.purely scan (corr (ma r) (std r)) &
            drop 2 &
            drop 100 &
            take n &
            toList_

    -- P.putStrLn ("average correlation of " <> P.show avCorr <> " should be half of " <> P.show c <> " ok!?" :: Text)
    P.writeFile "other/answer.md"
        ("$\av_{i=1}^{" <> P.show n <> "} corr = " <>
         P.show avCorr <> "$")

    rc <-
        stdReg gen &
        take n &
        L.purely scan ((,) <$> alpha (ma r) <*> beta (ma r)) &
        drop 2 &
        toList_

    let a = P.fst <$> rc
    let b = P.snd <$> rc

    let lopts = [LineOptions 0.005 ublue, LineOptions 0.002 ugrey]
    fileSvg "other/alpha.svg" (600,300) $ withHud (#aspect .~ widescreen $ def)
        (lineChart lopts)
        [ P.drop 2 $ P.zipWith Pair [0..] a
        , P.take n $ P.zipWith Pair [0..] (P.repeat 0)]

    fileSvg "other/beta.svg" (600,300) $ withHud (#aspect .~ widescreen $ def)
        (lineChart lopts)
        [ P.drop 2 $ P.zipWith Pair [0..] b
        , P.take n $ P.zipWith Pair [0..] (P.repeat 1)]

    fileSvg "other/corrjump.svg" (600,300) $ withHud (#aspect .~ widescreen $ def)
        (lineChart lopts)
        [ P.drop 20 $ P.zipWith Pair [0..] corrL
        , P.take n $ P.zipWith Pair [0..] (P.repeat c)]

    auto <- autoEst gen n 1 0.9 0.9
    auto' <- autoEst gen n 1 0.0000001 0.9

    fileSvg "other/autocorr.svg" (600,300) $ withHud (#aspect .~ widescreen $ def)
        (lineChart lopts)
        [ P.drop 10 $ P.zipWith Pair [0..] auto, P.drop 10 $ P.zipWith Pair [0..] auto']

    scratch $ withHud (#aspect .~ widescreen $ def)
        (lineChart lopts)
        [ P.drop 10 $ P.zipWith Pair [0..] auto]

