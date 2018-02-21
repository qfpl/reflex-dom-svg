{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.Pos
  ( Pos
  , X
  , Y
  , CenterX
  , CenterY
  , AsPosX (..)
  , AsPosY (..)
  , AsPosCenterX (..)
  , AsPosCenterY (..)
  , makePointsProp
  )
  where

import           Control.Lens       (Choice, Iso', Optic', Prism', Profunctor,
                                     from, iso, prism', re, (^.), (^?), _Just,
                                     _Show)

import           Data.Text          (Text)
import           Data.Text.Lens     (packed, unpacked)

import           Data.Semigroup     ((<>))

import           Data.List.NonEmpty (NonEmpty)

data X
data Y

data CenterX
data CenterY

newtype Pos p = Pos Float

posIso :: Iso' Float (Pos p)
posIso = iso Pos (\(Pos f) -> f)

posTextPrism :: Prism' Text (Pos p)
posTextPrism = prism'
  (^. from posIso . re _Show . packed)
  (^? unpacked . _Show . _Just . posIso)

class AsPosX p f s where
  posX :: Optic' p f s (Pos X)

class AsPosY p f s where
  posY :: Optic' p f s (Pos Y)

class AsPosCenterX p f s where
  posCenterX :: Optic' p f s (Pos CenterX)

class AsPosCenterY p f s where
  posCenterY :: Optic' p f s (Pos CenterY)

instance AsPosX p f (Pos X) where
  posX = id
instance AsPosY p f (Pos Y) where
  posY = id

instance (Profunctor p, Functor f) => AsPosX p f Float where
  posX = posIso
instance (Profunctor p, Functor f) => AsPosY p f Float where
  posY = posIso

instance (Choice p, Applicative f) => AsPosX p f Text where
  posX = posTextPrism
instance (Choice p, Applicative f) => AsPosY p f Text where
  posY = posTextPrism

instance AsPosCenterX p f (Pos CenterX) where
  posCenterX = id
instance AsPosCenterY p f (Pos CenterY) where
  posCenterY = id

instance (Profunctor p, Functor f) => AsPosCenterX p f Float where
  posCenterX = posIso
instance (Profunctor p, Functor f) => AsPosCenterY p f Float where
  posCenterY = posIso

instance (Choice p, Applicative f) => AsPosCenterX p f Text where
  posCenterX = posTextPrism
instance (Choice p, Applicative f) => AsPosCenterY p f Text where
  posCenterY = posTextPrism

makePointsProp
  :: NonEmpty (Pos X, Pos Y)
  -> Text
makePointsProp = foldMap
  (\(x,y) -> x ^. re posX <> "," <> y ^. re posY <> " ")
