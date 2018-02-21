{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.Radius
  ( Radius
  , _RadiusX
  , _RadiusY
  ) where

import           Control.Lens                    (Iso', Rewrapped, Wrapped (..),
                                                  iso, _Wrapped)

import           Reflex.Dom.Widget.SVG.Types.Pos (X, Y)

newtype Radius p =
  Radius Float

instance (Radius p) ~ t => Rewrapped (Radius p) t

instance Wrapped (Radius p) where
  type Unwrapped (Radius p) = Float
  _Wrapped' = iso (\(Radius x) -> x) Radius

_RadiusX :: Iso' (Radius X) Float
_RadiusX = _Wrapped

_RadiusY :: Iso' (Radius Y) Float
_RadiusY = _Wrapped
