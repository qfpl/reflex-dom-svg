{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

import           Reflex.Dom                      ((=:))

import           Reflex.Dom.Widget.SVG.Types.Pos (Pos, X, Y, makePointsProp)

data SVG_PolyLine = SVG_PolyLine
  { _svg_polyLine_start :: (Pos X, Pos Y)
  , _svg_polyLine_path  :: NonEmpty (Pos X, Pos Y)
  }

svg_polyLine_path :: Lens' SVG_PolyLine (NonEmpty (Pos X, Pos Y))
svg_polyLine_path f (SVG_PolyLine x1 x2)
  = fmap (SVG_PolyLine x1) (f x2)
{-# INLINE svg_polyLine_path #-}

svg_polyLine_start :: Lens' SVG_PolyLine (Pos X, Pos Y)
svg_polyLine_start f (SVG_PolyLine x1 x2)
  = fmap (`SVG_PolyLine` x2) (f x1)
{-# INLINE svg_polyLine_start #-}

makePolyLineProps
  :: SVG_PolyLine
  -> Map Text Text
makePolyLineProps SVG_PolyLine {..} =
  "points" =: makePointsProp (_svg_polyLine_start <| _svg_polyLine_path)
