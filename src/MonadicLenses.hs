{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module MonadicLenses where

import Control.Monad
import Control.Lens

import Data.Ix
import Data.Array.MArray
import Data.Array.IO

class RightModule m f where
  act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
  act = join
 
instance RightModule m (Const a) where
  act = Const . getConst

type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

monadicLens :: forall m f s t a b . (Functor f, RightModule m f) 
  => (s -> a) -> (s -> b -> m t) -> LensLike f s t a b
monadicLens v u k s = act (fmap (u s) (k (v s)))

update :: (Ix i) => MonadicLens IO (IOUArray i Double) () (i -> IO Double) (i, Double)
update = monadicLens @IO readArray (uncurry . writeArray)

{-}
-- Why doesn't this work ?????
update' :: (MArray a e m, Ix i, Functor f, RightModule m f) 
  => LensLike f (a i e) () (a i e) (i, e)
update' (a :: MArray a e m) 
  = act @m (fmap (uncurry (writeArray a)) (k a))
-}

learningRate :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double)
learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {y <- f i; pure (i, alpha*x + (1 - alpha)*y)}

bellman :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double, i)
bellman discountFactor = monadicLens @IO id $ \f (i, x, j) -> do {y <- f j; pure (i, x + discountFactor*y)}

environment :: (state -> action -> IO (state, Double)) -> MonadicLens IO (state, action) (state, Double) () ()
environment f = monadicLens @IO (const ()) (const . uncurry f)

greedy :: (Enum action) => ((state, action) -> IO Double) -> state -> IO action
greedy = undefined
