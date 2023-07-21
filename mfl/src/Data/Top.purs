module Data.Top where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

data Top = Top

derive instance Generic Top _
instance Show Top where show = genericShow
instance Eq Top where eq x = genericEq x
instance Ord Top where compare x = genericCompare x

