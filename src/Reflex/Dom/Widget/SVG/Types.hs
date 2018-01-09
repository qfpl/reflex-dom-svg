{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.Widget.SVG.Types where

import           Control.Lens       (Choice, Iso', Optic', Prism', Profunctor,
                                     iso, makeLenses, makeWrapped, prism', re,
                                     (^.), (^?), _1, _2, _Just, _Show, _Wrapped)

import           Data.Function      ((&))

import           Data.Text          (Text, unpack)
import           Data.Text.Lens     (packed)

import           Data.Map           (Map)
import qualified Data.Map           as Map

import           Data.List.NonEmpty (NonEmpty, (<|))
import           Data.Semigroup     ((<>))

import           Reflex.Dom         ((=:))

import           Safe               (readMay)

data X
data Y

data CenterX
data CenterY

newtype Pos p = Pos Float
makeWrapped ''Pos

posIso :: Iso' Float (Pos p)
posIso = iso Pos (\(Pos f) -> f)

posTextPrism :: Prism' Text (Pos p)
posTextPrism = prism'
  (\(Pos f) -> f ^. re _Show . packed)
  (fmap Pos . readMay . unpack)

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

newtype Width =
  Width Float
makeWrapped ''Width

newtype Height =
  Height Float
makeWrapped ''Height

newtype AttributeName = AttributeName Text
makeWrapped ''AttributeName

newtype AnimFrom = AnimFrom Int
makeWrapped ''AnimFrom

newtype AnimTo = AnimTo Int
makeWrapped ''AnimTo

newtype AnimDur = AnimDur Text
makeWrapped ''AnimDur

newtype RepeatCount = RepeatCount Text
makeWrapped ''RepeatCount

data SVG_El = SVG_El
  { _svg_root_width  :: Width
  , _svg_root_height :: Height
  }
makeLenses ''SVG_El

-- Basic shapes
data SVG_Rect = SVG_Rect
  { _svg_rect_pos_x          :: Pos X
  , _svg_rect_pos_y          :: Pos Y
  , _svg_rect_width          :: Width
  , _svg_rect_height         :: Height
  , _svg_rect_cornerRadius_x :: Maybe (CornerRadius X)
  , _svg_rect_cornerRadius_y :: Maybe (CornerRadius Y)
  }
makeLenses ''SVG_Rect

data SVG_Circle = SVG_Circle
  { _svg_circle_pos_centerX :: Pos CenterX
  , _svg_circle_pos_centerY :: Pos CenterY
  , _svg_circle_radius      :: Radius ()
  }
makeLenses ''SVG_Circle

data SVG_Animate = SVG_Animate
  { _svg_animate_attributeName :: AttributeName
  , _svg_animate_from          :: AnimFrom
  , _svg_animate_to            :: AnimTo
  , _svg_animate_dur           :: AnimDur
  , _svg_animate_repeatCount   :: RepeatCount
  }
makeLenses ''SVG_Animate

data SVG_Ellipse = SVG_Ellipse
  { _svg_ellipse_radius_x :: Radius X
  , _svg_ellipse_radius_y :: Radius Y
  , _svg_ellipse_center_x :: Pos CenterX
  , _svg_ellipse_center_y :: Pos CenterY
  }
makeLenses ''SVG_Ellipse

data SVG_Line = SVG_Line
  { _svg_line_pos_start :: ( Pos X, Pos Y )
  , _svg_line_pos_end   :: ( Pos X, Pos Y )
  }
makeLenses ''SVG_Line

data SVG_PolyLine = SVG_PolyLine
  { _svg_polyLine_start :: (Pos X, Pos Y)
  , _svg_polyLine_path  :: NonEmpty (Pos X, Pos Y)
  }
makeLenses ''SVG_PolyLine

data SVG_Polygon = SVG_Polygon
  { _svg_polygon_start :: (Pos X, Pos Y)
  , _svg_polygon_path  :: NonEmpty (Pos X, Pos Y)
  }
makeLenses ''SVG_Polygon

makePointsProp
  :: NonEmpty (Pos X, Pos Y)
  -> Text
makePointsProp = foldMap
  (\(x,y) -> x ^. re posX <> "," <> y ^. re posY <> " ")

makeCircleProps
  :: SVG_Circle
  -> Map Text Text
makeCircleProps c = Map.fromList
  [ ("cx", c ^. svg_circle_pos_centerX . re posCenterX)
  , ("cy", c ^. svg_circle_pos_centerY . re posCenterY)
  , ("r",  c ^. svg_circle_radius . re radius)
  ]

makeEllipseProps
  :: SVG_Ellipse
  -> Map Text Text
makeEllipseProps e = Map.fromList
  [ ("cx", e ^. svg_ellipse_center_x . re posCenterX)
  , ("cx", e ^. svg_ellipse_center_y . re posCenterY)
  , ("rx", e ^. svg_ellipse_radius_x . re radiusX)
  , ("ry", e ^. svg_ellipse_radius_y . re radiusY)
  ]

makeSVGLineProps
  :: SVG_Line
  -> Map Text Text
makeSVGLineProps l = Map.fromList
  [ ("x1", l ^. svg_line_pos_start . _1 . re posX)
  , ("y1", l ^. svg_line_pos_start . _2 . re posY)
  , ("x2", l ^. svg_line_pos_end . _1 . re posX)
  , ("y2", l ^. svg_line_pos_end . _2 . re posY)
  ]
makeSVGPolyLineProps
  :: SVG_PolyLine
  -> Map Text Text
makeSVGPolyLineProps SVG_PolyLine {..} =
  "points" =: makePointsProp (_svg_polyLine_start <| _svg_polyLine_path)

makeSVGPolygonProps
  :: SVG_Polygon
  -> Map Text Text
makeSVGPolygonProps SVG_Polygon {..} =
  "points" =: makePointsProp (_svg_polygon_start <| _svg_polygon_path)

makeAnimateProps
  :: SVG_Animate
  -> Map Text Text
makeAnimateProps a = Map.fromList
  [ ("attributeName", a ^. svg_animate_attributeName . _Wrapped )
  , ("from",          a ^. svg_animate_from . _Wrapped . re _Show . packed)
  , ("to",            a ^. svg_animate_to . _Wrapped . re _Show . packed)
  , ("dur",           a ^. svg_animate_dur . _Wrapped)
  , ("repeatCount",   a ^. svg_animate_repeatCount . _Wrapped)
  ]

makeRectProps
  :: SVG_Rect
  -> Map Text Text
makeRectProps r = Map.fromList
  [ ("x",      r ^. svg_rect_pos_x . re posX)
  , ("y",      r ^. svg_rect_pos_y . re posY)
  , ("width",  r ^. svg_rect_width . _Wrapped . re _Show . packed)
  , ("height", r ^. svg_rect_height . _Wrapped . re _Show . packed)
  ]
  & Map.alter (\_ -> r ^? svg_rect_cornerRadius_x . _Just . re cornerRadiusX) "rx"
  & Map.alter (\_ -> r ^? svg_rect_cornerRadius_y . _Just . re cornerRadiusY) "ry"

makeSVGProps
  :: SVG_El
  -> Map Text Text
makeSVGProps s = Map.fromList
  [ ("width", s ^. svg_root_width . _Wrapped . re _Show . packed )
  , ("height", s ^. svg_root_height . _Wrapped . re _Show . packed )
  , ("xmlns", "http://www.w3.org/2000/svg" )
  ]
