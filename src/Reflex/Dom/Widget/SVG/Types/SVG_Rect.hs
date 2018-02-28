{-# LANGUAGE OverloadedStrings #-}
-- | Types and lenses for the \<rect\> SVG element.
module Reflex.Dom.Widget.SVG.Types.SVG_Rect
  ( SVG_Rect (..)
  , svg_rect_pos_x
  , svg_rect_pos_y
  , svg_rect_width
  , svg_rect_height
  , svg_rect_cornerRadius_x
  , svg_rect_cornerRadius_y
  , makeRectProps
  )
  where

import           Control.Lens                             (Lens', at, ix, (.~),
                                                           (^.), (^?), _Just)

import           Data.Function                            ((&))

import           Data.Map                                 (Map)

import           Data.Text                                (Text)

import           Reflex.Dom.Widget.SVG.Types.CornerRadius (CornerRadius)
import           Reflex.Dom.Widget.SVG.Types.Pos          (Pos, X, Y)

import           Reflex.Dom.Widget.SVG.Types.Internal     (Height, Width,
                                                           wrappedToText)

-- | SVG <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect \<rect\>> properties
data SVG_Rect = SVG_Rect
  { _svg_rect_pos_x          :: Pos X -- ^ Top left X position
  , _svg_rect_pos_y          :: Pos Y -- ^ Top left Y position
  , _svg_rect_width          :: Width
  , _svg_rect_height         :: Height
  , _svg_rect_cornerRadius_x :: Maybe (CornerRadius X) -- ^ Optional rounded corner radius
  , _svg_rect_cornerRadius_y :: Maybe (CornerRadius Y) -- ^ Optional rounded corner radius
  }

-- | Lens for the X corner radius
svg_rect_cornerRadius_x :: Lens' SVG_Rect (Maybe (CornerRadius X))
svg_rect_cornerRadius_x f (SVG_Rect x1 x2 x3 x4 x5 x6)
  = fmap (\y1 -> SVG_Rect x1 x2 x3 x4 y1 x6) (f x5)
{-# INLINE svg_rect_cornerRadius_x #-}

-- | Lens for the Y corner radius
svg_rect_cornerRadius_y :: Lens' SVG_Rect (Maybe (CornerRadius Y))
svg_rect_cornerRadius_y f (SVG_Rect x1 x2 x3 x4 x5 x6)
  = fmap (SVG_Rect x1 x2 x3 x4 x5) (f x6)
{-# INLINE svg_rect_cornerRadius_y #-}

-- | Lens for the @Width@ of a @SVG_Rect@
svg_rect_width :: Lens' SVG_Rect Width
svg_rect_width f (SVG_Rect x1 x2 x3 x4 x5 x6)
  = fmap (\y1 -> SVG_Rect x1 x2 y1 x4 x5 x6) (f x3)
{-# INLINE svg_rect_width #-}

-- | Lens for the @Height@ of a @SVG_Rect@
svg_rect_height :: Lens' SVG_Rect Height
svg_rect_height f (SVG_Rect x1 x2 x3 x4 x5 x6)
  = fmap (\y1 -> SVG_Rect x1 x2 x3 y1 x5 x6) (f x4)
{-# INLINE svg_rect_height #-}

-- | Lens for the @Pos X@ of a @SVG_Rect@
svg_rect_pos_x :: Lens' SVG_Rect (Pos X)
svg_rect_pos_x f (SVG_Rect x1 x2 x3 x4 x5 x6)
  = fmap (\y1 -> SVG_Rect y1 x2 x3 x4 x5 x6) (f x1)
{-# INLINE svg_rect_pos_x #-}

-- | Lens for the @Pos Y@ of a @SVG_Rect@
svg_rect_pos_y :: Lens' SVG_Rect (Pos Y)
svg_rect_pos_y f (SVG_Rect x1 x2 x3 x4 x5 x6)
  = fmap (\y1 -> SVG_Rect x1 y1 x3 x4 x5 x6) (f x2)
{-# INLINE svg_rect_pos_y #-}

-- | Convert the given properties to the correct attributes for a \<rect\>.
makeRectProps
  :: SVG_Rect
  -> Map Text Text
makeRectProps r = mempty
  & ix "x" .~ r ^. svg_rect_pos_x . wrappedToText
  & ix "y" .~ r ^. svg_rect_pos_y . wrappedToText
  & ix "width" .~ r ^. svg_rect_width . wrappedToText
  & ix "height" .~ r ^. svg_rect_height . wrappedToText
  & at "rx" .~ r ^? svg_rect_cornerRadius_x . _Just . wrappedToText
  & at "ry" .~ r ^? svg_rect_cornerRadius_y . _Just . wrappedToText
