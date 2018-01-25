{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Baldursholur where

import Data.Kind

data SBool :: Bool -> Type where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

f :: SBool 'True
f = _


class Collection xs where
  type Elem xs :: Type
  cinsert :: Elem xs -> xs -> xs

instance Collection [a] where
  type Elem [a] = a
  cinsert :: a -> [a] -> [a]
  cinsert = undefined


k :: Maybe Integer
k = _ 3


g :: String
g = cinsert _ "hello"


h :: String
h = show (_ (_ :: Show a => a))


data Showable :: Type where
  Showable :: Show a => a -> Showable


k2 = Showable _
