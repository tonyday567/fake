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
import Streaming.Prelude
import Online
import qualified NumHask.Prelude as P
import NumHask.Prelude ((<*>), (<$>), Maybe(..), IO, (&), ($), (.), div, Double, (>>), Integer, (<>), (.), fromMaybe)
import Chart
-- import Diagrams.Backend.SVG
-- import Text.Pretty.Simple (pPrint)
import Control.Lens hiding (Wrapped, (&), (:>), Unwrapped)
import Data.Generics.Labels()

-- [hoogle](https://www.stackage.org/package/hoogle)

-- scratch :: Chart b -> IO ()
-- scratch = fileSvg "other/scratchpad.svg" (600,400)

l1d :: [Double] -> Chart b
l1d = withHud_ def sixbyfour (lineChart [LineOptions 0.002 (ucolor 0.5 0.8 0.5 1)]) . (:[]) . P.zipWith Pair [0..]
 
lopts :: [LineOptions]
lopts =
  P.zipWith
    (\x y -> LineOptions x (withOpacity (d3Colors1 y) 0.6))
    [0.001, 0.001, 0.001]
    [0,1,2]

s1ds :: P.FilePath -> Stream (Of [Double]) IO () -> IO ()
s1ds f s = do
    xs <- toList_ $ take 10000 s
    fileSvg f def $ withHud_ def sixbyfour (lineChart lopts) $
       lineOneD <$> P.transpose xs

data Opts w = Opts
    { streamMax :: w ::: Maybe Integer <?> "typical size of data stream"
    , testCorr :: w ::: Maybe Double <?> "test correlation"
    , rateCorr :: w ::: Maybe Double <?> "corr rate"
    , periodAuto :: w ::: Maybe Double <?> "period of an auto-dependent ma"
    , betaAuto  :: w ::: Maybe Double <?> "beta for auto-dependency on an ma"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "online-random"
    let n = P.maybe 10000 P.fromIntegral (streamMax o)
    let c = fromMaybe 0.8 (testCorr o)
    let r = fromMaybe 0.99 (rateCorr o)
    let pa = fromMaybe 0.01 (periodAuto o)
    let ba = fromMaybe 1 (betaAuto o)
    gen <- create

    rc <-
        stdReg gen &
        take n &
        L.purely scan ((,) <$> alpha (ma r) <*> beta (ma r)) &
        drop 2 &
        toList_

    let a = P.fst <$> rc
    let b = P.snd <$> rc

    fileSvg "other/alpha.svg" (#size .~ (Pair 600 300 :: Pair Double) $ (def :: SvgOptions)) $ withHud_
        ( #titles .~
        [ (def, "actual = " <> P.show 0)
        , (def, "alpha estimate")
        ]
        $ def)
        widescreen
        (lineChart lopts)
        [ P.drop 2 $ P.zipWith Pair [0..] a
        , P.take n $ P.zipWith Pair [0..] (P.repeat 0)]

    fileSvg "other/beta.svg" (#size .~ Pair 600 300 $ def) $ withHud_
        ( #titles .~
        [ (def, "actual = " <> P.show 1)
        , (def, "beta estimate")
        ]
        $ def)
        widescreen
        (lineChart lopts)
        [ P.drop 2 $ P.zipWith Pair [0..] b
        , P.take n $ P.zipWith Pair [0..] (P.repeat 1)]

    corrL <-
            rvsp_ gen (repeat 0 & take (n `div` 2) >> repeat c) &
            L.purely scan (corr (ma r) (std r)) &
            drop 2 &
            drop 100 &
            take n &
            toList_

    fileSvg "other/corrjump.svg" (#size .~ Pair 600 300 $ def) $ withHud_
        ( #titles .~
        [ (def, "correlation stream jump")
        ]
        $ def)
        widescreen
        (lineChart lopts)
        [ P.drop 20 $ P.zipWith Pair [0..] corrL
        , P.take n $ P.zipWith Pair [0..] (P.repeat c)]

    auto <- depMo
           (DepMoments (repeat [0,1,0,1,0,0])
             [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9]
             (rvStd gen)
             (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5])) &
            take n &
            L.purely scan (autocorr (ma r) (corr (ma r) (std r))) &
            drop 3 &
            toList_

    fileSvg "other/autocorr.svg" (#size .~ Pair 600 300 $ def) $
        withHud_
        ( #titles .~
        [ (def, "beta = " <> P.show ba <>
            "period = " <> P.show pa <> "r = " <> P.show r)
        , (def, "auto-correlation estimate")
        ]
        $ def)
        widescreen
        (lineChart lopts)
        [ P.drop 10 $ P.zipWith Pair [0..] auto, P.drop 10 $
          P.zipWith Pair [0..] auto]


