{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.Internal
  ( Width (..)
  , Height (..)
  , AttributeName (..)
  , RepeatCount (..)
  , wrappedToText
  ) where

import           Control.Lens   (Contravariant, Rewrapped, Wrapped (..), iso,
                                 to, _Wrapped)

import           Data.Text      (Text)
import           Data.Text.Lens (IsText, packed)

newtype Width         = Width Float
newtype Height        = Height Float
newtype AttributeName = AttributeName Text

instance Width ~ t => Rewrapped Width t
instance Wrapped Width where
  type Unwrapped Width = Float
  _Wrapped' = iso (\(Width x) -> x) Width

instance Height ~ t => Rewrapped Height t
instance Wrapped Height where
  type Unwrapped Height = Float
  _Wrapped' = iso (\(Height x) -> x) Height

instance AttributeName ~ t => Rewrapped AttributeName t
instance Wrapped AttributeName where
  type Unwrapped AttributeName = Text
  _Wrapped' = iso (\ (AttributeName x) -> x) AttributeName

data RepeatCount
  = NumOfTimes Word
  | Indefinite

instance Show RepeatCount where
  show (NumOfTimes n) = show n
  show Indefinite     = "indefinite"

wrappedToText
  :: ( Unwrapped t ~ Unwrapped s
     , IsText t1
     , Contravariant f
     , Functor f
     , Rewrapped t s
     , Rewrapped s t
     , Show (Unwrapped s)
     )
  => (t1 -> f t1)
  -> s
  -> f t
wrappedToText =
  _Wrapped . to show . packed
