{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.Pos
  ( Pos
  , X
  , Y
  , CenterX
  , CenterY
  , _PosX
  , _PosY
  , _PosCenterX
  , _PosCenterY
  , makePointsProp
  )
  where

import           Control.Lens                         (Iso', Rewrapped,
                                                       Wrapped (..), iso, (^.),
                                                       _Wrapped)

import           Data.Text                            (Text)

import           Data.Semigroup                       ((<>))

import           Data.List.NonEmpty                   (NonEmpty)

import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)

data X
data Y

data CenterX
data CenterY

newtype Pos p =
  Pos Float

instance (Pos p) ~ t => Rewrapped (Pos p) t

instance Wrapped (Pos p) where
  type Unwrapped (Pos p) = Float
  _Wrapped' = iso (\(Pos x) -> x) Pos

_PosX :: Iso' (Pos X) Float
_PosX = _Wrapped

_PosY :: Iso' (Pos Y) Float
_PosY = _Wrapped

_PosCenterX :: Iso' (Pos CenterX) Float
_PosCenterX = _Wrapped

_PosCenterY :: Iso' (Pos CenterY) Float
_PosCenterY = _Wrapped

makePointsProp
  :: NonEmpty (Pos X, Pos Y)
  -> Text
makePointsProp = foldMap
  (\(x,y) -> (x ^. wrappedToText) <> "," <> (y ^. wrappedToText) <> " ")
