{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Widget.SVG.Types.SVG_Ellipse where

import           Control.Lens                         (Lens', ix, (.~), (^.))

import           Data.Function                        ((&))
import           Data.Map                             (Map)

import           Data.Text                            (Text)

import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Pos      (CenterX, CenterY, Pos, X,
                                                       Y)
import           Reflex.Dom.Widget.SVG.Types.Radius   (Radius)





data SVG_Ellipse = SVG_Ellipse
  { _svg_ellipse_radius_x :: Radius X
  , _svg_ellipse_radius_y :: Radius Y
  , _svg_ellipse_center_x :: Pos CenterX
  , _svg_ellipse_center_y :: Pos CenterY
  }

svg_ellipse_center_x :: Lens' SVG_Ellipse (Pos CenterX)
svg_ellipse_center_x f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (\y1 -> SVG_Ellipse x1 x2 y1 x4) (f x3)
{-# INLINE svg_ellipse_center_x #-}

svg_ellipse_center_y :: Lens' SVG_Ellipse (Pos CenterY)
svg_ellipse_center_y f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (SVG_Ellipse x1 x2 x3) (f x4)
{-# INLINE svg_ellipse_center_y #-}

svg_ellipse_radius_x :: Lens' SVG_Ellipse (Radius X)
svg_ellipse_radius_x f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (\y1 -> SVG_Ellipse y1 x2 x3 x4) (f x1)
{-# INLINE svg_ellipse_radius_x #-}

svg_ellipse_radius_y :: Lens' SVG_Ellipse (Radius Y)
svg_ellipse_radius_y f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (\y1 -> SVG_Ellipse x1 y1 x3 x4) (f x2)
{-# INLINE svg_ellipse_radius_y #-}

makeEllipseProps
  :: SVG_Ellipse
  -> Map Text Text
makeEllipseProps e = mempty
  & ix "cx" .~ e ^. svg_ellipse_center_x . wrappedToText
  & ix "cx" .~ e ^. svg_ellipse_center_y . wrappedToText
  & ix "rx" .~ e ^. svg_ellipse_radius_x . wrappedToText
  & ix "ry" .~ e ^. svg_ellipse_radius_y . wrappedToText
