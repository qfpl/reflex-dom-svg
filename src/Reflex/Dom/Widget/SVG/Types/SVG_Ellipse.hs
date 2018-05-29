{-# LANGUAGE OverloadedStrings #-}
-- | Types and functions for the \<ellipse\> SVG element.
module Reflex.Dom.Widget.SVG.Types.SVG_Ellipse
  ( SVG_Ellipse (..)
  , svg_ellipse_radius_x
  , svg_ellipse_radius_y
  , svg_ellipse_center_x
  , svg_ellipse_center_y
  , makeEllipseProps
  ) where

import           Control.Lens                         (Lens', at, (?~), (^.))

import           Data.Function                        ((&))
import           Data.Map                             (Map)

import           Data.Text                            (Text)

import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Pos      (CenterX, CenterY, Pos, X,
                                                       Y)
import           Reflex.Dom.Widget.SVG.Types.Radius   (Radius)

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/ellipse \<ellipse\>> element.
data SVG_Ellipse = SVG_Ellipse
  { _svg_ellipse_radius_x :: Radius X
  , _svg_ellipse_radius_y :: Radius Y
  , _svg_ellipse_center_x :: Pos CenterX
  , _svg_ellipse_center_y :: Pos CenterY
  }
  deriving (Eq, Show)

-- | Lens for the Center X position of an @SVG_Ellipse@
svg_ellipse_center_x :: Lens' SVG_Ellipse (Pos CenterX)
svg_ellipse_center_x f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (\y1 -> SVG_Ellipse x1 x2 y1 x4) (f x3)
{-# INLINE svg_ellipse_center_x #-}

-- | Lens for the Center Y position of an @SVG_Ellipse@
svg_ellipse_center_y :: Lens' SVG_Ellipse (Pos CenterY)
svg_ellipse_center_y f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (SVG_Ellipse x1 x2 x3) (f x4)
{-# INLINE svg_ellipse_center_y #-}

-- | Lens for the Radius along the X axis of an @SVG_Ellipse@
svg_ellipse_radius_x :: Lens' SVG_Ellipse (Radius X)
svg_ellipse_radius_x f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (\y1 -> SVG_Ellipse y1 x2 x3 x4) (f x1)
{-# INLINE svg_ellipse_radius_x #-}

-- | Lens for the Radius along the Y axis of an @SVG_Ellipse@
svg_ellipse_radius_y :: Lens' SVG_Ellipse (Radius Y)
svg_ellipse_radius_y f (SVG_Ellipse x1 x2 x3 x4)
  = fmap (\y1 -> SVG_Ellipse x1 y1 x3 x4) (f x2)
{-# INLINE svg_ellipse_radius_y #-}

-- | Convert the given properties to the correct attributes for a \<ellipse\>.
makeEllipseProps
  :: SVG_Ellipse
  -> Map Text Text
makeEllipseProps e = mempty
  & at "cx" ?~ e ^. svg_ellipse_center_x . wrappedToText
  & at "cx" ?~ e ^. svg_ellipse_center_y . wrappedToText
  & at "rx" ?~ e ^. svg_ellipse_radius_x . wrappedToText
  & at "ry" ?~ e ^. svg_ellipse_radius_y . wrappedToText
