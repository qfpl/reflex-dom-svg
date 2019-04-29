{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Main functions for creating SVG dom elements via Reflex
module Reflex.Dom.Widget.SVG
  ( AsSVGTag (..)
  , BasicSVG (..)
  , BasicInner (..)
  , SVG_Root (..)
  , CanBeNested
  , SVGEl (..)
  , svg_
  , svgBasicDyn
  , svgBasicDyn_
  , svgElDynAttr'
  , svgElDynAttr_
  ) where

import           Control.Monad.Fix           (MonadFix)
import           Data.Text                   (Text)

import           Reflex                      (Dynamic, MonadHold) 
import qualified Reflex                      as R

import           Reflex.Dom.Core             (Element, EventResult, DomBuilderSpace, DomBuilder, PostBuild)
import qualified Reflex.Dom.Core             as RD

import           Data.Map                    (Map)

import           Reflex.Dom.Widget.SVG.Types (SVG_El, makeSVGProps)

-- | Lawless class to provide a constraint indicating that a given type is capable
-- of being represented by a SVG XML Tag. <rect>, <circle>, <svg>, etc.
class AsSVGTag s where
  svgTagName :: s -> Text

instance AsSVGTag BasicSVG where
  svgTagName Rectangle = "rect"
  svgTagName Circle    = "circle"
  svgTagName Ellipse   = "ellipse"
  svgTagName Path      = "path"
  svgTagName Line      = "line"
  svgTagName PolyLine  = "polyline"
  svgTagName Polygon   = "polygon"

instance AsSVGTag BasicInner where
  svgTagName Animate = "animate"

instance AsSVGTag SVG_Root where
  svgTagName SVG_Root = "svg"

-- | The SVG Root element: "<svg>"
data SVG_Root = SVG_Root

-- | The basic SVG shapes.
data BasicSVG
  = Rectangle
  | Circle
  | Ellipse
  | Path
  | Line
  | PolyLine
  | Polygon
  deriving (Show, Eq)

-- | The simplest inner element for a basic shape, the "<animate>" tag.
data BasicInner
  = Animate
  deriving (Eq, Ord)

-- | Create a relationship between a set of SVG tags that can be nested inside a
-- different set of SVG tags. Currently this just creates the relationship
-- between the "<animate>" tag and the basic shapes ("<rect>", "<circle>", etc).
type family CanBeNested a :: *
type instance CanBeNested BasicSVG = BasicInner

-- | This represents an SVG element, containing both the raw Reflex.Dom @El@ type
-- and a @Dynamic@ of all of the children that are nested in this element.
data SVGEl t m a = SVGEl
  { _svgEl_el       :: Element EventResult (DomBuilderSpace m) t
  , _svgEl_children :: Dynamic t (Map (CanBeNested a) (Element EventResult (DomBuilderSpace m) t))
  }

svgXMLNamespace :: Text
svgXMLNamespace = "http://www.w3.org/2000/svg"

-- | This is for creating a SVG element with @Dynamic@ attributes, and ensuring we
-- use the right namespace so the browser actually picks up on it. The name
-- space in use is "http://www.w3.org/2000/svg".
svgElDynAttr'
  :: forall t m a e. ( DomBuilder t m
                     , PostBuild t m
                     , AsSVGTag e
                     )
  => e
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
svgElDynAttr' = RD.elDynAttrNS'
  (Just svgXMLNamespace)
  . svgTagName

-- | As per @svgElDynAttr'@, but does not have any children.
svgElDynAttr_
  :: forall t m e. ( DomBuilder t m
                   , PostBuild t m
                   , AsSVGTag e
                   )
  => e
  -> Dynamic t (Map Text Text)
  -> m (Element EventResult (DomBuilderSpace m) t)
svgElDynAttr_ t dAttrs = fst <$> RD.elDynAttrNS'
  (Just svgXMLNamespace)
  (svgTagName t)
  dAttrs
  RD.blank

-- | Create the Root SVG element.
--
-- Note that there are not restrictions on the inner element, apart from the
-- return type being of @m (SVGEl t a)@. So you are free to place whatever you
-- like in there, but bear in mind that the browser rules for SVG are still in
-- play. So text inputs etc, won't work.
svg_
  :: ( DomBuilder t m
     , PostBuild t m
     , R.Reflex t
     , AsSVGTag a
     )
  => Dynamic t SVG_El
  -> m ( SVGEl t m a )
  -> m ( Element EventResult (DomBuilderSpace m) t, SVGEl t m a)
svg_ dAttrs =
  svgElDynAttr' SVG_Root (makeSVGProps <$> dAttrs)

-- | Create a SVG element that has dynamic attributes and contains children that
-- are acceptable children for this element. "<rect>" as a Basic Shape can only
-- contain "<animate>" elements, for example.
--
-- The SVG element will have some @Dynamic@ properties and a function that
-- allows these properties to be converted into a @Map Text Text@, inline with
-- other Reflex.Dom widgets.
svgBasicDyn
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m 
     , MonadHold t m
     , AsSVGTag s
     , AsSVGTag (CanBeNested s)
     , Ord (CanBeNested s)
     )
  => s
  -> ( p -> Map Text Text )
  -> Dynamic t p
  -> Dynamic t ( Map (CanBeNested s) (Map Text Text) )
  -> m ( SVGEl t m s )
svgBasicDyn t propFn dProps dInnerElMap =
  fmap ( uncurry SVGEl ) . svgElDynAttr' t (propFn <$> dProps) $ RD.listWithKey dInnerElMap
    (\innerS dInnerAttrs -> fst <$> svgElDynAttr' innerS dInnerAttrs RD.blank)

-- | As per the @svgBasicDyn@ function, except with no inner elements.
svgBasicDyn_
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , AsSVGTag s
     , AsSVGTag (CanBeNested s)
     , Ord (CanBeNested s)
     )
  => s
  -> ( p -> Map Text Text )
  -> Dynamic t p
  -> m ( SVGEl t m s )
svgBasicDyn_ t propFn dProps =
  svgBasicDyn t propFn dProps (pure mempty)
