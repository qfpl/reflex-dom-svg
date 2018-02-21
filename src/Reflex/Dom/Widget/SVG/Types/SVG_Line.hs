{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Widget.SVG.Types.SVG_Line where

import           Control.Lens                    (Lens', ix, re, (.~), (^.), _1,
                                                  _2)

import           Data.Function                   ((&))

import           Data.Map                        (Map)

import           Data.Text                       (Text)

import           Reflex.Dom.Widget.SVG.Types.Pos (AsPosX (..), AsPosY (..), Pos,
                                                  X, Y)

data SVG_Line = SVG_Line
  { _svg_line_pos_start :: ( Pos X, Pos Y )
  , _svg_line_pos_end   :: ( Pos X, Pos Y )
  }

svg_line_pos_end :: Lens' SVG_Line (Pos X, Pos Y)
svg_line_pos_end f (SVG_Line x1 x2)
  = fmap (SVG_Line x1) (f x2)
{-# INLINE svg_line_pos_end #-}

svg_line_pos_start :: Lens' SVG_Line (Pos X, Pos Y)
svg_line_pos_start f (SVG_Line x1 x2)
  = fmap (`SVG_Line` x2) (f x1)
{-# INLINE svg_line_pos_start #-}

makeSVGLineProps
  :: SVG_Line
  -> Map Text Text
makeSVGLineProps l = mempty
  & ix "x1" .~ l ^. svg_line_pos_start . _1 . re posX
  & ix "y1" .~ l ^. svg_line_pos_start . _2 . re posY
  & ix "x2" .~ l ^. svg_line_pos_end . _1 . re posX
  & ix "y2" .~ l ^. svg_line_pos_end . _2 . re posY
