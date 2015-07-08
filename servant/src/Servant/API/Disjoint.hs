{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Servant.API.Disjoint ((:<!>)) where

import Servant.API.Alternative
import Servant.API.Sub
import Data.Type.Equality
import Data.Type.Bool

type family IsDisjoint left right :: Bool where
  IsDisjoint (leftLeft :<|> leftRight) right = IsDisjoint leftLeft right && IsDisjoint leftRight right
  IsDisjoint left (rightLeft :<|> rightRight) = IsDisjoint left rightLeft && IsDisjoint left rightRight
  IsDisjoint (leftPath :> left) (rightPath :> right) = (Not (leftPath == rightPath) || IsDisjoint left right)
  IsDisjoint (leftPath :> left) right = True
  IsDisjoint left (rightPath :> right) = True
  IsDisjoint left right = False

type Disjoint left right = (IsDisjoint left right ~ True)

type left :<!> right = (Disjoint left right) => left :<|> right
