{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Widget.SVG.Types.SVG_Circle where

import           Control.Lens                       (Lens', ix, re, (.~), (^.))

import           Data.Function                      ((&))

import           Data.Map                           (Map)

import           Data.Text                          (Text)

import           Reflex.Dom.Widget.SVG.Types.Pos    (AsPosCenterX (..),
                                                     AsPosCenterY (..), CenterX,
                                                     CenterY, Pos)
import           Reflex.Dom.Widget.SVG.Types.Radius (AsRadius (..), Radius)

data SVG_Circle = SVG_Circle
  { _svg_circle_pos_centerX :: Pos CenterX
  , _svg_circle_pos_centerY :: Pos CenterY
  , _svg_circle_radius      :: Radius ()
  }

svg_circle_pos_centerX :: Lens' SVG_Circle (Pos CenterX)
svg_circle_pos_centerX f (SVG_Circle x1 x2 x3)
  = fmap (\y1 -> SVG_Circle y1 x2 x3) (f x1)
{-# INLINE svg_circle_pos_centerX #-}

svg_circle_pos_centerY :: Lens' SVG_Circle (Pos CenterY)
svg_circle_pos_centerY f (SVG_Circle x1 x2 x3)
  = fmap (\y1 -> SVG_Circle x1 y1 x3) (f x2)
{-# INLINE svg_circle_pos_centerY #-}

svg_circle_radius :: Lens' SVG_Circle (Radius ())
svg_circle_radius f (SVG_Circle x1 x2 x3)
  = fmap (SVG_Circle x1 x2) (f x3)
{-# INLINE svg_circle_radius #-}

makeCircleProps
  :: SVG_Circle
  -> Map Text Text
makeCircleProps c = mempty
  & ix "cx" .~ c ^. svg_circle_pos_centerX . re posCenterX
  & ix "cy" .~ c ^. svg_circle_pos_centerY . re posCenterY
  & ix "r"  .~ c ^. svg_circle_radius . re radius
