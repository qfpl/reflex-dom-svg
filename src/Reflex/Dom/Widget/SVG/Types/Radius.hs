{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.Radius where

import           Control.Lens                    (Choice, Optic', iso, prism',
                                                  re, (^.), _Show)

import           Data.Text                       (Text, unpack)
import           Data.Text.Lens                  (packed)
import           Safe                            (readMay)

import           Reflex.Dom.Widget.SVG.Types.Pos (X, Y)

newtype Radius p =
  Radius Float

class AsRadius t p f s where
  radius :: Optic' p f s (Radius t)

instance (Choice p, Applicative f) => AsRadius t p f Float where
  radius = iso Radius (\(Radius f) -> f)

instance (Choice p, Applicative f) => AsRadius t p f Text where
  radius = prism' (\(Radius f) -> f ^. re _Show . packed) (fmap Radius . readMay . unpack)

radiusX :: AsRadius X p f s => Optic' p f s (Radius X)
radiusX = radius

radiusY :: AsRadius Y p f s => Optic' p f s (Radius Y)
radiusY = radius
