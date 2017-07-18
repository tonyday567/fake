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
import Streaming.Prelude hiding (delay)
import Streaming.Internal
import Online
import qualified NumHask.Prelude as P
import NumHask.Prelude (Maybe(..), IO, (&), ($), (.), (+), (*), (/), div, Double, (>>), Integer, (<>))
import Chart hiding (Wrapped, Unwrapped, each, (:>), Getter)
import Control.Monad.Primitive (PrimState)
import Text.Pretty.Simple (pPrint)
import qualified Data.Sequence as Seq

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

delay1 :: L.Fold a (Maybe a)
delay1 = L.Fold (\_ a -> Just a) Nothing P.identity
-- just delaying the stream via `yield P.nan >> s` say doesn't really work, as the copy . _1 . unseparate trick streams the nan first.  Streams are not zippy by ordering when the logic gets loopy

delay_ :: P.Int -> L.Fold a (Maybe a)
delay_ n = L.Fold (\x a -> Seq.drop 1 $ x Seq.|> Just a) (Seq.fromList $ P.replicate n Nothing) ((\case { Seq.EmptyL -> Nothing; (x Seq.:< _) -> x}) . Seq.viewl)

delay :: (P.BoundedField a) => P.Int -> L.Fold a a
delay n = P.fmap (P.fromMaybe P.nan) (delay_ (n+1))

-- | branching pipe
-- each [1..4] & branch (L.purely scan L.sum) (map (*10)) & eitherToPair & toList_
branch :: (P.Functor f, P.Monad (t (Stream f m2)), P.Monad m1, P.Monad m2, P.Monad m, S.MFunctor t, S.MonadTrans t) => (t (Stream f m2) r2 -> Stream (Of a1) (Stream (Of b) m1) r1) -> (Stream (Of a) (Stream (Of a) m) r -> Stream f (t m2) r2) -> Stream (Of a) m r -> Stream (Of (P.Either a1 b)) m1 r1
branch m0 m1 s = s & copy & m1 & distribute & m0 & unseparate & maps sumToEither

eitherToPair :: (P.Monad m) => Stream (Of (P.Either Double Double)) m r -> Stream (Of (Double,Double)) m r
eitherToPair s = loop Nothing Nothing s where
  loop stateL stateR str = case str of
    Return r -> P.return r
    Effect m -> Effect (P.liftM (loop stateL stateR) m)
    Step (P.Left l :> rest) -> case stateR of
      Nothing -> loop (Just l) Nothing rest
      Just r' -> do
        yield (l,r')
        loop Nothing Nothing rest
    Step (P.Right r :> rest) -> case stateL of
      Nothing -> loop Nothing (Just r) rest
      Just l' -> do
        yield (l',r)
        loop Nothing Nothing rest


maPipe :: (P.Functor f, P.Monad (t (Stream f m2)), P.Monad m1, P.Monad m2, P.Monad m, S.MFunctor t, S.MonadTrans t) => (t (Stream f m2) r2 -> Stream (Of a1) (Stream (Of a1) m1) r1) -> (Stream (Of a) (Stream (Of a) m) r -> Stream f (t m2) r2) -> Stream (Of a) m r -> Stream (Of a1) m1 r1
maPipe m0 m1 s = s & copy & m1 & distribute & m0 & unseparate & maps unify
  where
    unify :: S.Sum (Of a) (Of a) m -> Of a m
    unify (S.InL a) = a
    unify (S.InR a) = a

-- | scan0 (\_ a -> a) P.identity P.identity == P.identity
scan0 :: P.Monad m => (x -> a -> x) -> (a -> x) -> (x -> b) -> Stream (Of a) m r -> Stream (Of b) m r
scan0 acc beginf done s = do
    n <- S.lift $ next s
    case n of
      P.Left r -> P.return r
      (P.Right (a, rest)) ->
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
                   L.handles _2 (L.handles (filtered (P.not . P.isNaN)) (ma r)) &
                    P.fmap (\(x',y') -> (x',if P.isNaN y' then 0 else y'))) &
    drop 2 &
    (zipWith (\b' (x',y) -> x'+b'*y) b)

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "testing fake data"
    -- let n = P.fromMaybe 10000 (P.fromIntegral <$> streamMax o)
    let c = P.fromMaybe 0.8 (testCorr o)
    let r = P.fromMaybe 0.99 (rateCorr o)
    gen <- create
    pure ()

-- (n,eff) <- rvsp_ gen (repeat 0.5) & L.purely scan (corr 0.99) & drop 3 & copy & take 1000 & toList_ <&> take 100 <&> l1d <&> scratch & length <&> lazily
{-
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

-}


\end{code}

```include
other/answer.md
```

Other Steaming Examples
---

- https://github.com/ElvishJerricco/kleisli-functors/blob/d0bde122c1d0c988b16d3737bba712931b25c963/src/Control/Kleisli/Functor.hs
- https://github.com/jwiegley/notes/blob/f15aa380ddf98bc387b24a66171a62b38f236079/haskell/Teletype.hs
- https://github.com/ejconlon/freeing/blob/422748981e5fc76a4aa3bf1d25eca479e4c54085/src/Freeing.hs
- https://github.com/Tr1p0d/code-snippets/blob/2403ae3e97c3b4f8e27fcd5cb96b432c4f4ea0e4/ea/src/GeneticPipeline/GeneticPipeline.hs

<div class="footer">

Love to [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/), [pandoc](http://pandoc.org/), [mathjax](https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference) and [doctest](https://www.stackage.org/package/doctest).
</div>
