> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> module Example where

> import Control.Lens ((^.), (.~), (^?), (+~), ix, from)
> import Data.Function ((&))
> import Data.Monoid (mempty)
> import Data.Semigroup (mappend)
> import Reflex (Dynamic)
> import Reflex.Dom (MonadWidget, (=:))

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
>     shiftRect = fmap (S.svg_rect_pos_x . S._PosX +~ 70.0)

We can also define a ``<rect>`` with corner radius.

>     dRect3 = pure $ S.SVG_Rect
>       (S._PosX # 20.0)
>       (S._PosY # 20.0)
>       (S.Width 50.0)
>       (S.Height 50.0)
>       (15.0 ^? from S._CornerRadiusX)
>       (15.0 ^? from S._CornerRadiusY)

This is the same as <rect x="20" y="20" height="50" width="50" rx="15" ry="15">.

Build a ``<circle>``.

>     dCircle = pure $ S.SVG_Circle
>       (S._PosCenterX # 200.0)
>       (S._PosCenterY # 200.0)
>       (S._Radius # 70.0)

This is the same as writing: <circle cx="200" cy="200" r="70">.

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
>     _ <- S.svgBasicDyn_ S.Circle S.makeCircleProps dCircle
>     S.svgBasicDyn S.Rectangle S.makeRectProps dRect3 ( pure $ S.Animate =: dAnim )
>
>   pure ()
