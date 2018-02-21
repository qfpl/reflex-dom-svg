{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.CornerRadius where

import           Control.Lens                    (Choice, Optic', iso, prism',
                                                  re, (^.), _Show)

import           Data.Text                       (Text, unpack)
import           Data.Text.Lens                  (packed)
import           Safe                            (readMay)

import           Reflex.Dom.Widget.SVG.Types.Pos (X, Y)

newtype CornerRadius p =
  CornerRadius Float

class AsCornerRadius t p f s where
  cornerRadius :: Optic' p f s (CornerRadius t)

instance AsCornerRadius t p f (CornerRadius t) where
  cornerRadius = id

instance (Choice p, Applicative f) => AsCornerRadius t p f Float where
  cornerRadius = iso CornerRadius (\(CornerRadius f) -> f)

instance (Choice p, Applicative f) => AsCornerRadius t p f Text where
  cornerRadius = prism' (\(CornerRadius f) -> f ^. re _Show . packed) (fmap CornerRadius . readMay . unpack)

cornerRadiusX :: AsCornerRadius X p f s => Optic' p f s (CornerRadius X)
cornerRadiusX = cornerRadius

cornerRadiusY :: AsCornerRadius Y p f s => Optic' p f s (CornerRadius Y)
cornerRadiusY = cornerRadius
