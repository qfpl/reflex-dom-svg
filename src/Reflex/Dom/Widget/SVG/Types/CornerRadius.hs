{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.CornerRadius
  ( CornerRadius
  , _CornerRadiusX
  , _CornerRadiusY
  , cornerRadiusToText
  ) where

import           Control.Lens                    (Iso', iso)

import           Data.Text                       (Text, pack)

import           Reflex.Dom.Widget.SVG.Types.Pos (X, Y)

newtype CornerRadius p =
  CornerRadius Float

cornerRadiusIso :: Iso' (CornerRadius p) Float
cornerRadiusIso = iso (\(CornerRadius f) -> f) CornerRadius

cornerRadiusToText :: CornerRadius p -> Text
cornerRadiusToText (CornerRadius p) = pack (show p)

_CornerRadiusX :: Iso' (CornerRadius X) Float
_CornerRadiusX = cornerRadiusIso

_CornerRadiusY :: Iso' (CornerRadius Y) Float
_CornerRadiusY = cornerRadiusIso
