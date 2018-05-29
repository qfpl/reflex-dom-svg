{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Types and functions for the \<polyline\> SVG element.
--
-- The \<polyline\> SVG element is an SVG basic shape that creates straight lines
-- connecting several points. Typically a polyline is used to create open shapes
-- as the last point doesn't have to be connected to the first point. For closed
-- shapes see the \<polygon\> element.
module Reflex.Dom.Widget.SVG.Types.SVG_PolyLine
  ( SVG_PolyLine (..)
  , svg_polyLine_path
  , svg_polyLine_start
  , makePolyLineProps
  ) where

import           Control.Lens                    (Lens')

import           Data.Map                        (Map)

import           Data.Text                       (Text)

import           Data.List.NonEmpty              (NonEmpty, (<|))

import           Reflex.Dom.Core                 ((=:))

import           Reflex.Dom.Widget.SVG.Types.Pos (Pos, X, Y, makePointsProp)

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/polyline \<polyline\>> element.
data SVG_PolyLine = SVG_PolyLine
  { _svg_polyLine_start :: (Pos X, Pos Y)
  , _svg_polyLine_path  :: NonEmpty (Pos X, Pos Y)
  }
  deriving (Eq, Show)

-- | Lens for the list of @(Pos X, Pos Y)@ of an @SVG_PolyLine@ path attribute.
svg_polyLine_path :: Lens' SVG_PolyLine (NonEmpty (Pos X, Pos Y))
svg_polyLine_path f (SVG_PolyLine x1 x2)
  = fmap (SVG_PolyLine x1) (f x2)
{-# INLINE svg_polyLine_path #-}

-- | Lens for the starting @(Pos X, Pos Y)@ of an @SVG_PolyLine@ element.
svg_polyLine_start :: Lens' SVG_PolyLine (Pos X, Pos Y)
svg_polyLine_start f (SVG_PolyLine x1 x2)
  = fmap (`SVG_PolyLine` x2) (f x1)
{-# INLINE svg_polyLine_start #-}

-- | Convert the given properties to the correct 'points' attribute of a \<polygon\>.
makePolyLineProps
  :: SVG_PolyLine
  -> Map Text Text
makePolyLineProps SVG_PolyLine {..} =
  "points" =: makePointsProp (_svg_polyLine_start <| _svg_polyLine_path)
