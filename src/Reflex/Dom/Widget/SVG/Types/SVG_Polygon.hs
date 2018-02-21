{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Reflex.Dom.Widget.SVG.Types.SVG_Polygon where

import           Control.Lens                    (Lens')

import           Data.List.NonEmpty              (NonEmpty, (<|))
import           Data.Map                        (Map)
import           Data.Text                       (Text)

import           Reflex.Dom                      ((=:))

import           Reflex.Dom.Widget.SVG.Types.Pos (Pos, X, Y, makePointsProp)

data SVG_Polygon = SVG_Polygon
  { _svg_polygon_start :: (Pos X, Pos Y)
  , _svg_polygon_path  :: NonEmpty (Pos X, Pos Y)
  }

svg_polygon_path :: Lens' SVG_Polygon (NonEmpty (Pos X, Pos Y))
svg_polygon_path f (SVG_Polygon x1 x2) = fmap (SVG_Polygon x1) (f x2)
{-# INLINE svg_polygon_path #-}

svg_polygon_start :: Lens' SVG_Polygon (Pos X, Pos Y)
svg_polygon_start f (SVG_Polygon x1 x2) = fmap (`SVG_Polygon` x2) (f x1)
{-# INLINE svg_polygon_start #-}

makeSVGPolygonProps
  :: SVG_Polygon
  -> Map Text Text
makeSVGPolygonProps SVG_Polygon {..} =
  "points" =: makePointsProp (_svg_polygon_start <| _svg_polygon_path)
