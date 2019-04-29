{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Types and functions for the \<animate\> SVG element.
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
                                                       at, iso, prism, to, (?~),
                                                       (^.), _Wrapped)

import           Data.Function                        ((&))

import           Data.Semigroup                       ((<>))

import           Data.Text                            (Text, pack)
import           Data.Text.Lens                       (packed)

import           Data.Map                             (Map)

import           GHC.Word                             (Word16)

import           Reflex.Dom.Widget.SVG.Types.Internal (AttributeName (..),
                                                       RepeatCount (..),
                                                       wrappedToText)

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from from> attribute
newtype AnimFrom      = AnimFrom Word16 deriving (Eq, Show)

-- | <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to to> attribute
newtype AnimTo        = AnimTo Word16 deriving (Eq, Show)

instance AnimFrom ~ t => Rewrapped AnimFrom t
instance Wrapped AnimFrom where
  type Unwrapped AnimFrom = Word16
  _Wrapped' = iso (\ (AnimFrom x) -> x) AnimFrom

instance AnimTo ~ t => Rewrapped AnimTo t
instance Wrapped AnimTo where
  type Unwrapped AnimTo = Word16
  _Wrapped' = iso (\ (AnimTo x) -> x) AnimTo

-- | We don't allow for negative animation durations, and currently we're only
-- interested in animations that last a matter of seconds or milliseconds.
data AnimDuration
  = Secs Word16
  | MSecs Word16
  deriving (Eq, Show)

-- | Classy Prism set for the @AnimDuration@ type
class AsAnimDuration r where
  _AnimDuration :: Prism' r AnimDuration -- ^ General prism for when you have to deal with all of the constructors
  _Secs :: Prism' r Word16               -- ^ Prism for Seconds
  _MSecs :: Prism' r Word16              -- ^ Prism for Milliseconds
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

animDurationToText :: AnimDuration -> Text
animDurationToText (Secs s)   = toText "s" s
animDurationToText (MSecs ms) = toText "ms" ms

-- | Properties for the <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/animate \<animate\>> element.
data SVG_Animate = SVG_Animate
  { _svg_animate_attributeName :: AttributeName
  , _svg_animate_from          :: AnimFrom
  , _svg_animate_to            :: AnimTo
  , _svg_animate_dur           :: AnimDuration
  , _svg_animate_repeatCount   :: RepeatCount
  }
  deriving (Eq, Show)

-- | Lens for the @AttirbuteName@ of an @SVG_Animate@ element.
svg_animate_attributeName :: Lens' SVG_Animate AttributeName
svg_animate_attributeName f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate y1 x2 x3 x4 x5) (f x1)
{-# INLINE svg_animate_attributeName #-}

-- | Lens for the @AnimDuration@ of an @SVG_Animate@ element.
svg_animate_dur :: Lens' SVG_Animate AnimDuration
svg_animate_dur f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate x1 x2 x3 y1 x5) (f x4)
{-# INLINE svg_animate_dur #-}

-- | Lens for the @AnimFrom@ of an @SVG_Animate@ element.
svg_animate_from :: Lens' SVG_Animate AnimFrom
svg_animate_from f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate x1 y1 x3 x4 x5) (f x2)
{-# INLINE svg_animate_from #-}

-- | Lens for the @AnimTo@ of an @SVG_Animate@ element.
svg_animate_to :: Lens' SVG_Animate AnimTo
svg_animate_to f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (\y1 -> SVG_Animate x1 x2 y1 x4 x5) (f x3)
{-# INLINE svg_animate_to #-}

-- | Lens for the @AnimRepeatCount@ of an @SVG_Animate@ element.
svg_animate_repeatCount :: Lens' SVG_Animate RepeatCount
svg_animate_repeatCount f (SVG_Animate x1 x2 x3 x4 x5)
  = fmap (SVG_Animate x1 x2 x3 x4) (f x5)
{-# INLINE svg_animate_repeatCount #-}

-- | Convert the given properties to the correct attributes for a \<animate\>.
makeAnimateProps
  :: SVG_Animate
  -> Map Text Text
makeAnimateProps a = mempty
  & at "attributeName" ?~ a ^. svg_animate_attributeName . _Wrapped
  & at "from"          ?~ a ^. svg_animate_from . wrappedToText
  & at "to"            ?~ a ^. svg_animate_to . wrappedToText
  & at "dur"           ?~ a ^. svg_animate_dur . to animDurationToText
  & at "repeatCount"   ?~ a ^. svg_animate_repeatCount . to show . packed
