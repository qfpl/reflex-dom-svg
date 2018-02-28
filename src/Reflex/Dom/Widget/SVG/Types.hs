{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Contains the root \<svg\> element properties and re-exports all of the individual SVG Types
module Reflex.Dom.Widget.SVG.Types
  ( SVG_El (..)
  , svg_root_width
  , svg_root_height
  , makeSVGProps

  , module Reflex.Dom.Widget.SVG.Types.Internal

  , module Reflex.Dom.Widget.SVG.Types.CornerRadius
  , module Reflex.Dom.Widget.SVG.Types.Pos
  , module Reflex.Dom.Widget.SVG.Types.Radius

  , module Reflex.Dom.Widget.SVG.Types.SVG_Animate
  , module Reflex.Dom.Widget.SVG.Types.SVG_Circle
  , module Reflex.Dom.Widget.SVG.Types.SVG_Ellipse
  , module Reflex.Dom.Widget.SVG.Types.SVG_Line
  , module Reflex.Dom.Widget.SVG.Types.SVG_Polygon
  , module Reflex.Dom.Widget.SVG.Types.SVG_PolyLine
  , module Reflex.Dom.Widget.SVG.Types.SVG_Rect
  , module Reflex.Dom.Widget.SVG.Types.SVG_Path
  )
  where

import           Control.Lens                             (Lens', (^.))

import           Data.Text                                (Text)

import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map

import           Reflex.Dom.Widget.SVG.Types.Internal

import           Reflex.Dom.Widget.SVG.Types.SVG_Path

import           Reflex.Dom.Widget.SVG.Types.CornerRadius
import           Reflex.Dom.Widget.SVG.Types.Pos
import           Reflex.Dom.Widget.SVG.Types.Radius

import           Reflex.Dom.Widget.SVG.Types.SVG_Animate
import           Reflex.Dom.Widget.SVG.Types.SVG_Circle
import           Reflex.Dom.Widget.SVG.Types.SVG_Ellipse
import           Reflex.Dom.Widget.SVG.Types.SVG_Line
import           Reflex.Dom.Widget.SVG.Types.SVG_Polygon
import           Reflex.Dom.Widget.SVG.Types.SVG_PolyLine
import           Reflex.Dom.Widget.SVG.Types.SVG_Rect

-- | Minimum information required for building a SVG root element.
data SVG_El = SVG_El
  { _svg_root_width  :: Width
  , _svg_root_height :: Height
  }

-- | Lens for @Height@ attribute on @SVG_El@
svg_root_height :: Lens' SVG_El Height
svg_root_height f (SVG_El x1 x2) = fmap (SVG_El x1) (f x2)
{-# INLINE svg_root_height #-}

-- | Lens for @Width@ attribute on @SVG_El@
svg_root_width :: Lens' SVG_El Width
svg_root_width f (SVG_El x1 x2) = fmap (`SVG_El` x2) (f x1)
{-# INLINE svg_root_width #-}

-- | Convert the record to the correct attribute map for Reflex.
makeSVGProps
  :: SVG_El
  -> Map Text Text
makeSVGProps s = Map.fromList
  [ ("width", s ^. svg_root_width . wrappedToText )
  , ("height", s ^. svg_root_height . wrappedToText )
  , ("xmlns", "http://www.w3.org/2000/svg" )
  ]
