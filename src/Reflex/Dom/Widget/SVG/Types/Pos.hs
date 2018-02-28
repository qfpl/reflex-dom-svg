{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | @Pos@ is a general purpose wrapper for the various times we want to
-- indicate we're using a specific positional value.
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

-- | Types to help keep our respective coordinate types separate
data X
-- | Types to help keep our respective coordinate types separate
data Y
-- | Types to help keep our respective coordinate types separate
data CenterX
-- | Types to help keep our respective coordinate types separate
data CenterY

-- | Wrap up the normal @Float@ value with a @newtype@ so that we can't mix
-- things up as easily. Include a phantom type so we're able to be granular
-- about the specific position we're interested in.
newtype Pos p =
  Pos Float

instance (Pos p) ~ t => Rewrapped (Pos p) t

instance Wrapped (Pos p) where
  type Unwrapped (Pos p) = Float
  _Wrapped' = iso (\(Pos x) -> x) Pos

-- | Specific Iso for describing a X coordinate
_PosX :: Iso' (Pos X) Float
_PosX = _Wrapped

-- | Specific Iso for describing a Y coordinate
_PosY :: Iso' (Pos Y) Float
_PosY = _Wrapped

-- | Specific Iso for describing a centered X coordinate
_PosCenterX :: Iso' (Pos CenterX) Float
_PosCenterX = _Wrapped

-- | Specific Iso for describing a centered Y coordinate
_PosCenterY :: Iso' (Pos CenterY) Float
_PosCenterY = _Wrapped

-- | Convert the list of points to a correctly formatted list of X/Y positions
-- expected by SVG attributes.
makePointsProp
  :: NonEmpty (Pos X, Pos Y)
  -> Text
makePointsProp = foldMap
  (\(x,y) -> (x ^. wrappedToText) <> "," <> (y ^. wrappedToText) <> " ")
