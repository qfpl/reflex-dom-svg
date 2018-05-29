{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Miscellaneous types and functions of the API. This module is named Internal
-- as it is possible it will change.
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

-- | Wrap the @Float@ value with something more meaningful.
newtype Width         = Width Float deriving (Eq, Show)
-- | Wrap the @Float@ value with something more meaningful.
newtype Height        = Height Float deriving (Eq, Show)
-- | Wrap the @Text@ value with something more meaningful.
newtype AttributeName = AttributeName Text deriving (Eq, Show)

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

-- | Capture the information about the <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/repeatCount repeatCount> attribute.
data RepeatCount
  = NumOfTimes Word
  | Indefinite
  deriving Eq

instance Show RepeatCount where
  show (NumOfTimes n) = show n
  show Indefinite     = "indefinite"

-- | Helper function to convert a @Wrapped@ value to a @Text@ value.
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
