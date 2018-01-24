{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies,
             UndecidableInstances, ConstraintKinds #-}
module ONotation where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality


-- We define a very simplistic O notation, with sufficient expressiveness
-- to capture the complexity of a few simple sorting algorithms
data AsymptoticPoly = NLogN Nat Nat

-- Synonyms for common terms
type N     = NLogN 1 0
type LogN  = NLogN 0 1
type One   = NLogN 0 0

-- Just to be able to write it nicely
type O (a :: AsymptoticPoly) = a

type family (^.) (n :: AsymptoticPoly) (m :: Nat) :: AsymptoticPoly where
  (NLogN a b) ^. n = (NLogN (a * n) (b * n))

type family (*.) (n :: AsymptoticPoly) (m :: AsymptoticPoly) :: AsymptoticPoly where
  (NLogN a b) *. (NLogN c d) = NLogN (a+c) (b+d)
  
type family OCmp (n :: AsymptoticPoly) (m :: AsymptoticPoly) :: Ordering where
  OCmp (NLogN a b) (NLogN c d) = If (CmpNat a c == EQ) (CmpNat b d) (CmpNat a c)

type family OGEq (n :: AsymptoticPoly) (m :: AsymptoticPoly) :: Bool where
  OGEq n m = Not (OCmp n m == 'LT)

type (>=.) n m = OGEq n m ~ True

infix 4 >=.
infixl 7 *., ^.
