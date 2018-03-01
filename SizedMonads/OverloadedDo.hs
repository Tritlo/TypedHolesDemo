{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Prelude
import qualified Prelude as P

import GHC.TypeLits
import Data.Proxy
import Data.Kind
import Data.Type.Bool
import Data.Typeable

import Sized



data ApplicativeDoableDict f
    = ApplicativeF (forall a f . Applicative f => a -> f a) (forall f a b . Applicative f => f (a -> b) -> f a -> f b)
    | SizedApplicativeF (forall a f . SizedApplicative f => a -> f 0 a) (forall f a b k i j . (SizedApplicative f, k ~ Max i j) => f i (a -> b) -> f j a -> f k b)
    deriving (Typeable)

data DoableDict m
  = Monadic (ApplicativeDoableDict m) (forall m a b . Monad m => m a -> (a -> m b) -> m b)
  | SizedMonadic (ApplicativeDoableDict m) (forall m a b i j . SizedMonad m => m i a -> (a -> m j b) -> m (i+j) b)
    deriving (Typeable)

class ApplicativeDoable f where
  adoDict :: ApplicativeDoableDict f

class  ApplicativeDoable m => Doable m where
  doDict :: DoableDict m

instance {-# OVERLAPPABLE #-} Applicative f => ApplicativeDoable f where
  adoDict = ApplicativeF P.pure (P.<*>)

instance {-# OVERLAPPABLE #-} (Typeable m, Monad m) => Doable m where
 doDict = Monadic adoDict (P.>>=)

instance {-# OVERLAPPING #-} SizedApplicative f => ApplicativeDoable (f (n :: Nat)) where
  adoDict = SizedApplicativeF spure (|<*>)

instance {-# OVERLAPPING #-} (Typeable (f n), SizedMonad f) => Doable (f (n :: Nat)) where
  doDict = SizedMonadic adoDict (|>>=)

one :: () -> SizedIO 1 ()
one _ = wrapWithCost (Proxy :: Proxy 1) $ putStrLn "one"

two :: () -> SizedT IO 2 ()
two _ = wrapWithCost (Proxy :: Proxy 2) $ putStrLn "two"

three :: () -> SizedIO 3 ()
three _ = wrapWithCost (Proxy :: Proxy 3) $ putStrLn "three"

t0a :: SizedIO 6 ()
t0a = do { x <- one ()
         ; y <- two x
         ; z <- three y
         ; () |<$ return (x,y,z)}
    where dd = doDict :: DoableDict (SizedIO 6)
          SizedMonadic (SizedApplicativeF return (<*>)) (>>=) = dd

t0b :: SizedIO 3 ()
t0b = do { x <- one ()
         ; y <- two ()
         ; z <- three ()
         ; () |<$ return (x,y,z)}
    where dd = doDict :: DoableDict (SizedIO 6)
          SizedMonadic (SizedApplicativeF return (<*>)) (>>=) = dd


t2 :: IO ()
t2 = do { x <- putStrLn "hey"
        ; print x }
     where dd = doDict :: DoableDict IO
           Monadic (ApplicativeF return (<*>)) (>>=) = dd

main :: IO ()
main = do { t2
          ; runSizedT t0a }
