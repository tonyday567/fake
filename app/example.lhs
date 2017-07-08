```include
other/header.md
```

fake
===

scratchpad
---

<img style="border:2px solid grey" src="other/scratch.svg">

alpha
---

<img style="border:2px solid grey" src="other/alpha.svg">

\begin{align}
\alpha & = \frac{\sum y \sum x^2 - \sum x \sum xy}{n\sum x^2 - (\sum x)^2} \\
      & = \frac{n^2 \bar{y} \overline{x^2} - n^2 \bar{x} \overline{xy}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
      & = \frac{\bar{y} \overline{x^2} - \bar{x} \overline{xy}}{\overline{x^2} - \bar{x}^2} \\

\end{align}


beta
---

<img style="border:2px solid grey" src="other/beta.svg">


\begin{align}
\beta & = \frac{n\sum xy - \sum x \sum y}{n\sum x^2 - (\sum x)^2} \\
      & = \frac{n^2 \overline{xy} - n^2 \bar{x} \bar{y}}{n^2 \overline{x^2} - n^2 \bar{x}^2} \\
      & = \frac{\overline{xy} - \bar{x} \bar{y}}{\overline{x^2} - \bar{x}^2} \\

\end{align}

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [mwc-random](https://www.stackage.org/package/mwc-random)
- [mwc-probability](https://www.stackage.org/package/mwc-probability)
- [streaming](https://www.stackage.org/package/streaming)
- [chart-unit](https://www.github.com/tonyday567/chart-unit)
- [numhask](https://www.github.com/tonyday567/numhask)

\begin{code}
import Options.Generic
import Fake.Data
import System.Random.MWC
import qualified Control.Foldl as L
import qualified Streaming as S
import Streaming.Prelude
import Streaming.Internal
import Online
import qualified NumHask.Prelude as P
import NumHask.Prelude (Maybe(..), IO, (&), ($), (.), (+), (*), (/), div, Double, (>>), Integer, (<>))
import Chart hiding (Wrapped, Unwrapped, each, (:>), Getter)
import Control.Monad.Primitive (PrimState)
import Text.Pretty.Simple (pPrint)

\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
data Opts w = Opts
    { streamMax :: w ::: Maybe Integer <?> "typical size of data stream"
    , testCorr :: w ::: Maybe Double <?> "test correlation"
    , rateCorr :: w ::: Maybe Double <?> "corr rate"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratch.svg" (600,400)

l1d :: [Double] -> QDiagram SVG V2 Double Any
l1d = withChart def (lineChart [LineConfig 0.002 (Color 0.5 0.8 0.5 1)]) . (:[]) . P.zipWith V2 [0..]

rvStd :: L.PrimMonad m => Gen (PrimState m) -> Stream (Of Double) m ()
rvStd gen = rv_ gen (repeat 0) (repeat 1)

stdReg :: L.PrimMonad m => Gen (PrimState m) -> Stream (Of (Double, Double)) m ()
stdReg gen = xy_ (repeat 0) (repeat 1) (rvStd gen) (rvStd gen)

foldid :: L.Fold Double Double
foldid = L.Fold (\_ a -> a) (0/0) P.identity

delay1 :: Stream (Of Double) IO () -> Stream (Of Double) IO ()
delay1 s = yield P.nan >> s

delay :: P.Int -> Stream (Of Double) IO () -> Stream (Of Double) IO ()
delay n s = repeat P.nan & take n >> s

dup :: (P.Monad m) => Stream (Of a) m r -> Stream (Of (a,a)) m r
dup = map (\x -> (x,x))

type Pipe a b m r = Stream (Of a) m r -> Stream (Of b) m r

scan0 :: P.Monad m => (x -> a -> x) -> (a -> x) -> (x -> b) -> Stream (Of a) m r -> Stream (Of b) m r
scan0 acc beginf done s = do
    n <- S.lift $ next s
    case n of
      P.Left r -> P.return r
      (P.Right (a, rest)) ->
          scan acc (beginf a) done rest


-- https://hackage.haskell.org/package/transformers-0.4.2.0/docs/Data-Functor-Sum.html
-- https://github.com/ElvishJerricco/kleisli-functors/blob/d0bde122c1d0c988b16d3737bba712931b25c963/src/Control/Kleisli/Functor.hs

-- https://github.com/jwiegley/notes/blob/f15aa380ddf98bc387b24a66171a62b38f236079/haskell/Teletype.hs

-- https://github.com/ejconlon/freeing/blob/422748981e5fc76a4aa3bf1d25eca479e4c54085/src/Freeing.hs

-- https://github.com/Tr1p0d/code-snippets/blob/2403ae3e97c3b4f8e27fcd5cb96b432c4f4ea0e4/ea/src/GeneticPipeline/GeneticPipeline.hs



id' :: (P.Monad m) => Pipe a a m r
id' = map P.identity
-- S.hoist S.lift (each [1..5]) :: Stream (Of P.Int) (P.StateT P.Int IO) ()
-- S.liftM (S.hoist S.lift) id'

-- firstS :: (P.Monad m) => Pipe b c m r -> Pipe (b,d) (c,d) m r
firstS :: (P.Monad m) =>
    (Stream (Of b) (P.StateT d m) r ->
      Stream (Of c) (P.StateT d m) r) ->
    Stream (Of (b, d)) (P.StateT d m) r ->
    Stream (Of (c, d)) (P.StateT d m) r
firstS p s = s & storeSnd & p & restoreSnd

firstS' p s = s & storeSnd & P.liftM (S.hoist S.lift) p & restoreSnd

t1 :: (P.Monad m) =>
    (Stream (Of a) m r ->
      Stream (Of b) m r) ->
    Stream (Of a) (P.StateT d m) r ->
    Stream (Of b) (P.StateT d m) r
t1 p s = P.undefined -- S.hoist S.lift

storeSnd :: (P.Monad m) => Stream (Of (b,d)) (P.StateT d m) r -> Stream (Of b) (P.StateT d m) r
storeSnd = loop where
  loop s = case s of
    Return r -> pure r
    Effect m -> Effect (P.liftM loop m)
    Step ((b,d) :> rest) -> Effect $ do
        P.put d
        pure (Step (b :> loop rest))

restoreSnd :: (P.Monad m) => Stream (Of b) (P.StateT d m) r -> Stream (Of (b,d)) (P.StateT d m) r
restoreSnd = loop where
  loop s = case s of
    Return r -> pure r
    Effect m -> Effect (P.liftM loop m)
    Step (b :> rest) -> Effect $ do
        d <- P.get
        pure (Step ((b,d) :> loop rest))




stdAuto ::
    Double ->
    Stream (Of Double) IO () ->
    Stream (Of Double) IO () ->
    Stream (Of Double) IO ()
stdAuto r b x = zipWith (\(x',max') b' -> x' + max' * b') (x & L.purely scan ((,) <$> foldid <*> ma r)) b

stdAuto' ::
    Double ->
    Stream (Of Double) IO () ->
    Stream (Of Double) IO () ->
    Stream (Of (Double, Double, Double, Double)) IO ()
stdAuto' r b x = zipWith3 (\x'' (x',max') b' -> (x'', x', max', b')) x (x & L.purely scan ((,) <$> foldid <*> ma r)) b

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "testing fake data"
    let n = P.fromMaybe 10000 (P.fromIntegral <$> streamMax o)
    let c = P.fromMaybe 0.8 (testCorr o)
    let r = P.fromMaybe 0.99 (rateCorr o)
    gen <- create

-- (n,eff) <- rvsp_ gen (repeat 0.5) & L.purely scan (corr 0.99) & drop 3 & copy & take 1000 & toList_ <&> take 100 <&> l1d <&> scratch & length <&> lazily

    avCorr <-
            rvsp_ gen (repeat 0 & take (n `P.div` 2) >> repeat c) &
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

    scratch $ withChart def (lineChart [LineConfig 0.002 (Color 0.5 0.8 0.5 1), LineConfig 0.002 (Color 0.5 0.5 0.5 1)]) [P.zipWith V2 [0..] corrL, P.take n $ P.zipWith V2 [0..] (P.repeat c)]

    P.putStrLn ("average correlation of " <> P.show avCorr <> " should be half of " <> P.show c <> " 👍" :: Text)
    P.writeFile "other/answer.md"
        ("$\av_{i=1}^{" <> P.show n <> "} corr = " <>
         P.show avCorr <> "$")


    rc <- stdReg gen & take n & L.purely scan ((,) <$> alpha (ma r) <*> beta (ma r)) & drop 2 & toList_

    let a = P.fst <$> rc
    let b = P.snd <$> rc

    fileSvg "other/beta.svg" (600,400) $ withChart def (lineChart [LineConfig 0.002 (Color 0.5 0.8 0.5 1), LineConfig 0.002 (Color 0.5 0.5 0.5 1)]) [P.zipWith V2 [0..] b, P.take n $ P.zipWith V2 [0..] (P.repeat 1)]


    fileSvg "other/alpha.svg" (600,400) $ withChart def (lineChart [LineConfig 0.002 (Color 0.5 0.8 0.5 1), LineConfig 0.002 (Color 0.5 0.5 0.5 1)]) [P.zipWith V2 [0..] a, P.take n $ P.zipWith V2 [0..] (P.repeat 0)]


    fileSvg "other/scratch.svg" (600,400) $ withChart def (lineChart [LineConfig 0.002 (Color 0.5 0.8 0.5 1), LineConfig 0.002 (Color 0.5 0.5 0.5 1)]) [P.zipWith V2 [0..] a, P.take n $ P.zipWith V2 [0..] b]


\end{code}

```include
other/answer.md
```

<div class="footer">

Love to [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/), [pandoc](http://pandoc.org/), [mathjax](https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference) and [doctest](https://www.stackage.org/package/doctest).
</div>
