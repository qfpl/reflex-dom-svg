> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> module Main where

> import Control.Lens ((^?), (+~), (?~), (#), from, at)
> import Data.Function ((&))
> import Data.Monoid (mempty, mappend)
> import Reflex (Dynamic)
> import Reflex.Dom (mainWidget)
> import Reflex.Dom.Core (MonadWidget, (=:))

> import qualified Reflex.Dom.Widget.SVG as S
> import Reflex.Dom.Widget.SVG.Types (SVG_Rect)
> import qualified Reflex.Dom.Widget.SVG.Types as S

> exampleUsage :: forall t m. ( MonadWidget t m ) => m ()
> exampleUsage = do
>   let

Construct the ``<svg>`` element.

>     dSvgProps = pure $ S.SVG_El
>       (S.Width 400)
>       (S.Height 300)
>       Nothing

Create a normal ``Map`` of HTML attributes to apply to the shape

>     attrs = mempty
>       & at "id" ?~ "svg1"
>       & at "class" ?~ "blue no-yellow"

Build our first ``<rect>``.

>     dRect1 = pure $ S.SVG_Rect
>       (S._PosX # 40.0)
>       (S._PosY # 40.0)
>       (S.Width 50.0)
>       (S.Height 50.0)
>       Nothing
>       Nothing

This is the same as writing: <rect x="40" y="40" height="50" width="50">.

We can use lenses to modify the properties of our shape.

>     shiftRect :: Dynamic t SVG_Rect -> Dynamic t SVG_Rect
>     shiftRect = fmap (S.svg_rect_pos_x . S._PosX +~ 3.0)

We can also define a ``<rect>`` with corner radius.

>     dRect3 = pure $ S.SVG_Rect
>       (S._PosX # 20.0)
>       (S._PosY # 20.0)
>       (S.Width 30.0)
>       (S.Height 30.0)
>       (15.0 ^? from S._CornerRadiusX)
>       (15.0 ^? from S._CornerRadiusY)

This is the same as <rect x="40" y="40" height="50" width="50" cx="15" cy="15">.

We can also build some ``Dynamic`` animation element properties:

>     dAnim = S.makeAnimateProps $ S.SVG_Animate
>       ( S.AttributeName "x" )
>       ( S.AnimFrom 10 )
>       ( S.AnimTo 100 )
>       ( S.Secs 10 )
>       ( S.Indefinite )

This is the same as having written: <animate attributeName="x" from="10" to="100" dur="10s" repeatCount="indefinite"/>

Finally, put it all together for ``Reflex.Dom`` to add to our page.

>   _ <- S.svg_ dSvgProps $ do
>     _ <- S.svgBasicDyn S.Rectangle (mappend attrs . S.makeRectProps) (shiftRect dRect1) (pure mempty)
>     _ <- S.svgBasicDyn_ S.Rectangle S.makeRectProps dRect1
>     S.svgBasicDyn S.Rectangle S.makeRectProps dRect3 ( pure $ S.Animate =: dAnim )
>
>   pure ()

> main :: IO ()
> main = mainWidget exampleUsage
