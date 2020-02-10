{-# LANGUAGE MonoLocalBinds #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Prelude as P
import Chart
import Control.Lens hiding (Wrapped, (:>), Unwrapped)
import Data.Generics.Labels()
import Online
import Online.Random
import Options.Generic
import Streaming.Prelude as S
import System.Random.MWC
import qualified Control.Foldl as L
import Codec.Picture.Types
import Data.Maybe
import Data.List (transpose)
import qualified Data.Text as Text
import Readme.Lhs

lopts :: [LineStyle]
lopts =
  P.zipWith (\w c -> defaultLineStyle & #color .~ c & #width .~ w)
  [0.001 :: Double, 0.001, 0.001]
  [ PixelRGB8 197 140 75
  , PixelRGB8 60 127 43
  , PixelRGB8 56 42 140
  ]

lopts0 = defaultLineStyle & #color .~ PixelRGB8 197 140 75 & #width .~ 0.001

data Opts w = Opts
    { streamMax :: w ::: Maybe Integer <?> "typical size of data stream"
    , dropFirst :: w ::: Maybe Integer <?> "drop first measurements"
    , testCorr :: w ::: Maybe Double <?> "test correlation"
    , rateCorr :: w ::: Maybe Double <?> "corr rate"
    , periodAuto :: w ::: Maybe Double <?> "period of an auto-dependent ma"
    , betaAuto  :: w ::: Maybe Double <?> "beta for auto-dependency on an ma"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

dm :: Gen L.RealWorld -> P.Int -> P.Int -> [Double] -> L.Fold Double a -> IO [a]
dm gen n d ss f = depMo
           (DepMoments (S.repeat ss)
             [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9]
             (rvStd gen)
             (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5])) &
            S.take n &
            L.purely scan f &
            S.drop (d + 1) &
            toList_

lineChart os ds = P.zipWith (\os' ds' -> Chart (LineA os') ds') os (P.fmap SpotPoint <$> ds)

writeLineChart :: P.FilePath -> [Text] -> [LineStyle] -> [[Point Double]] -> IO ()
writeLineChart fname ts os ds = 
  writeChartSvg fname (Point 600 300) $ hudChartSvg unitRect
    [((\t -> title (defaultTitle t)) <$> ts)]
    (lineChart os ds)

s' :: [Double] -> IO ()
s' ss = do
  g <- create
  c <- dm g 1000 200 ss ((\x y z -> [x,y,z]) <$> autocorr (ma r) (corr (ma r) (std r)) <*> std 0.999 <*> ma 0.999)
  writeLineChart "other/scratchpad.svg"
    [ Text.pack ("beta = " <> P.show ba <>
       " period = " <> P.show pa <> " r = " <> P.show r)
    , "auto-correlation estimate"]
    lopts
    (P.zipWith Point ts <$> transpose c)
        where
          pa = 0.01
          ba = 1
          r = 0.99
          d :: P.Int = 50
          n :: P.Int = 10000
          ts :: [Double] = P.fromIntegral <$> [d..n]

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "online-random"
    let d = 2 P.+ P.maybe 50 P.fromIntegral (dropFirst o)
    let n = P.maybe 10000 P.fromIntegral (streamMax o)
    let c = fromMaybe 0.8 (testCorr o)
    let r = fromMaybe 0.99 (rateCorr o)
    let pa = fromMaybe 0.01 (periodAuto o)
    let ba = fromMaybe 1 (betaAuto o)
    gen <- create

    rc <-
        stdReg gen &
        S.take n &
        L.purely scan ((,) <$> alpha (ma r) <*> beta (ma r)) &
        S.drop d &
        toList_

    let a = P.fst <$> rc
    let b = P.snd <$> rc
    let ts :: [Double] = P.fromIntegral <$> [d..n]

    writeLineChart "other/alpha.svg"
      [ Text.pack $ ("actual = " <> P.show 0)
      , "alpha estimate"]
      lopts
      [ P.zipWith Point ts a
      , P.take (n - d) $ P.zipWith Point ts (P.repeat 0)
      ]

    writeLineChart "other/beta.svg"
      [ Text.pack $ ("actual = " <> P.show 1)
      , "beta estimate"]
      lopts
      [ P.zipWith Point ts b
      , P.take (n - d) $ P.zipWith Point ts (P.repeat 1)
      ]

    corrL <-
            rvsp_ gen (S.repeat 0 & S.take (n `div` 2) >> S.repeat c) &
            L.purely scan (corr (ma r) (std r)) &
            S.take n &
            S.drop d &
            toList_

    writeLineChart "other/corrjump.svg"
      [ "correlation stream jump"]
      lopts
        [ P.zipWith Point ts corrL
        , P.take (n - d) $ P.zipWith Point ts (P.repeat c)
        ]

    auto <- depMo
           (DepMoments (S.repeat [0,1,0,1,0,0])
             [mconst 1, ma 0.9, std 0.9, mconst 1, ma 0.9, std 0.9]
             (rvStd gen)
             (P.zip [0..2] [0..2]) (P.zip [3..5] [3..5])) &
            S.take n &
            L.purely scan (autocorr (ma r) (corr (ma r) (std r))) &
            S.drop (d + 1) &
            toList_

    writeLineChart "other/autocorr.svg"
      [ (Text.pack $ "beta = " <> P.show ba <>
            " period = " <> P.show pa <> " r = " <> P.show r)
      , "auto-correlation estimate"
      ]
      lopts
        [ P.zipWith Point ts auto
        ]

    _ <- runOutput
      ("readme.md", GitHubMarkdown)
      ("index.html", Html)
      $ pure ()

    pure ()  

