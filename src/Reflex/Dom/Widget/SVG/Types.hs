{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Contains the root \<svg\> element properties and re-exports all of the individual SVG Types
module Reflex.Dom.Widget.SVG.Types
  ( SVG_El (..)
  , ViewBox (..)
  , svg_root_width
  , svg_root_height
  , svg_root_viewbox
  , viewBox_height
  , viewBox_width
  , viewBox_min_x
  , viewBox_min_y

  , makeSVGProps
  , makeViewBox

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

import           Control.Lens                             (Lens', at, to, (?~),
                                                           (^.), _Just,
                                                           _Wrapped)

import           Data.Function                            ((&))

import           Data.Text                                (Text)
import qualified Data.Text                                as Text

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

-- | SVG @viewBox@ attribute
data ViewBox = ViewBox
  { _viewBox_min_X  :: Float
  , _viewBox_min_Y  :: Float
  , _viewBox_width  :: Width
  , _viewBox_height :: Height
  }

-- | Lens for @_viewBox_min_X@ attribute on @ViewBox@
viewBox_min_x :: Lens' ViewBox Float
viewBox_min_x f (ViewBox minX minY w h) = fmap (\minX' -> ViewBox minX' minY w h) (f minX)
{-# INLINE viewBox_min_x #-}

-- | Lens for @_viewBox_min_Y@ attribute on @ViewBox@
viewBox_min_y :: Lens' ViewBox Float
viewBox_min_y f (ViewBox minX minY w h) = fmap (\minY' -> ViewBox minX minY' w h) (f minY)
{-# INLINE viewBox_min_y #-}

-- | Lens for @_viewBox_width@ attribute on @ViewBox@
viewBox_width :: Lens' ViewBox Width
viewBox_width f (ViewBox minX minY w h) = fmap (\w' -> ViewBox minX minY w' h) (f w)
{-# INLINE viewBox_width #-}

-- | Lens for @_viewBox_min_X@ attribute on @ViewBox@
viewBox_height :: Lens' ViewBox Height
viewBox_height f (ViewBox minX minY w h) = fmap (ViewBox minX minY w) (f h)
{-# INLINE viewBox_height #-}

-- | Minimum information required for building a SVG root element.
data SVG_El = SVG_El
  { _svg_root_width  :: Width
  , _svg_root_height :: Height
  , _svg_view_box    :: Maybe ViewBox
  }

-- | Lens for @Height@ attribute on @SVG_El@
svg_root_height :: Lens' SVG_El Height
svg_root_height f (SVG_El x1 x2 v) = fmap (\x2' -> SVG_El x1 x2' v) (f x2)
{-# INLINE svg_root_height #-}

-- | Lens for @Width@ attribute on @SVG_El@
svg_root_width :: Lens' SVG_El Width
svg_root_width f (SVG_El x1 x2 v) = fmap (\x1' -> SVG_El x1' x2 v) (f x1)
{-# INLINE svg_root_width #-}

-- | Lens for @ViewBox@ attribute on @SVG_El@
svg_root_viewbox :: Lens' SVG_El ( Maybe ViewBox )
svg_root_viewbox f (SVG_El x1 x2 v) = fmap (SVG_El x1 x2) (f v)
{-# INLINE svg_root_viewbox #-}

makeViewBox
  :: ViewBox
  -> Text
makeViewBox ViewBox {..} = Text.unwords $ Text.pack . show <$>
  [ _viewBox_min_X
  , _viewBox_min_Y
  , _viewBox_width ^. _Wrapped
  , _viewBox_height ^. _Wrapped
  ]

-- | Convert the record to the correct attribute map for Reflex.
makeSVGProps
  :: SVG_El
  -> Map Text Text
makeSVGProps s = Map.fromList
  [ ("width", s ^. svg_root_width . wrappedToText )
  , ("height", s ^. svg_root_height . wrappedToText )
  , ("xmlns", "http://www.w3.org/2000/svg" )
  ]
  & at "viewBox" ?~ (s ^. svg_root_viewbox . _Just . to makeViewBox)
