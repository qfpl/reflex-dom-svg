{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types.SVG_Animate
  ( AnimFrom (..)
  , AnimTo (..)
  , AnimDuration (..)
  , AsAnimDuration (..)
  , SVG_Animate (..)
  , svg_animate_attributeName
  , svg_animate_from
  , svg_animate_to
  , svg_animate_dur
  , svg_animate_repeatCount
  , makeAnimateProps
  ) where

import           Control.Lens                         (Lens', Prism', Rewrapped,
                                                       Unwrapped, Wrapped (..),
                                                       iso, ix, prism, re, to,
                                                       (.~), (^.), _Wrapped)

import           Data.Function                        ((&))

import           Data.Bifunctor                       (first)
import           Data.Semigroup                       ((<>))

import           Data.Text                            (Text, pack, unpack)
import           Data.Text.Lens                       (packed)

import           Safe                                 (readEitherSafe)

import           Data.Map                             (Map)

import           GHC.Word                             (Word16)

import           Reflex.Dom.Widget.SVG.Types.Internal (AttributeName (..),
                                                       RepeatCount (..),
                                                       wrappedToText)
newtype AnimFrom      = AnimFrom Word16
newtype AnimTo        = AnimTo Word16

instance AnimFrom ~ t => Rewrapped AnimFrom t
instance Wrapped AnimFrom where
  type Unwrapped AnimFrom = Word16
  _Wrapped' = iso (\ (AnimFrom x) -> x) AnimFrom

instance AnimTo ~ t => Rewrapped AnimTo t
instance Wrapped AnimTo where
  type Unwrapped AnimTo = Word16
  _Wrapped' = iso (\ (AnimTo x) -> x) AnimTo

data AnimDuration
  = Secs Word16
  | MSecs Word16

class AsAnimDuration r where
  _AnimDuration :: Prism' r AnimDuration
  _Secs :: Prism' r Word16
  _MSecs :: Prism' r Word16
  _Secs = _AnimDuration . _Secs
  _MSecs = _AnimDuration . _MSecs

instance AsAnimDuration AnimDuration where
  _AnimDuration = id
  _Secs = prism Secs
    (\case Secs d -> Right d
           x       -> Left x
    )
  _MSecs = prism MSecs
    (\case MSecs d -> Right d
           x        -> Left x
    )

toText :: String -> Word16 -> Text
toText s = pack . (<> s) . show

fromText :: Text -> Either Text Word16
fromText = first pack . readEitherSafe . unpack

instance AsAnimDuration Text where
  _Secs = prism (toText "s") fromText
  _MSecs = prism (toText "ms") fromText

data SVG_Animate = SVG_Animate
  { _svg_animate_attributeName :: AttributeName
  , _svg_animate_from          :: AnimFrom
  , _svg_animate_to            :: AnimTo
  , _svg_animate_dur           :: AnimDuration
  , _svg_animate_repeatCount   :: RepeatCount
  }

svg_animate_attributeName :: Lens' SVG_Animate AttributeName
svg_animate_attributeName f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate y1 x2 x3 x4 x5) (f x1)
{-# INLINE svg_animate_attributeName #-}

svg_animate_dur :: Lens' SVG_Animate AnimDuration
svg_animate_dur f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate x1 x2 x3 y1 x5) (f x4)
{-# INLINE svg_animate_dur #-}

svg_animate_from :: Lens' SVG_Animate AnimFrom
svg_animate_from f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate x1 y1 x3 x4 x5) (f x2)
{-# INLINE svg_animate_from #-}

svg_animate_repeatCount :: Lens' SVG_Animate RepeatCount
svg_animate_repeatCount f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (SVG_Animate x1 x2 x3 x4) (f x5)
{-# INLINE svg_animate_repeatCount #-}

svg_animate_to :: Lens' SVG_Animate AnimTo
svg_animate_to f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate x1 x2 y1 x4 x5) (f x3)
{-# INLINE svg_animate_to #-}

makeAnimateProps
  :: SVG_Animate
  -> Map Text Text
makeAnimateProps a = mempty
  & ix "attributeName" .~ a ^. svg_animate_attributeName . _Wrapped
  & ix "from"          .~ a ^. svg_animate_from . wrappedToText
  & ix "to"            .~ a ^. svg_animate_to . wrappedToText
  & ix "dur"           .~ a ^. svg_animate_dur . re _AnimDuration
  & ix "repeatCount"   .~ a ^. svg_animate_repeatCount . to show . packed
