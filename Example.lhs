> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Example where

> import Control.Lens ((^.), (.~), (^?), (+~), ix, from)
> import Control.Monad.Fix (MonadFix)
> import Data.Function ((&))
> import Data.Monoid (mempty)
> import Data.Semigroup (mappend)
> import qualified Reflex as R
> import Reflex.Dom ((=:))
> import qualified Reflex.Dom as RD

> import qualified Reflex.Dom.Widget.SVG as S
> import qualified Reflex.Dom.Widget.SVG.Types as S

> exampleUsage
>   :: ( R.Reflex t
>      , R.MonadHold t m
>      , RD.DomBuilderSpace m ~ RD.GhcjsDomSpace
>      , RD.DomBuilder t m
>      , RD.PostBuild t m
>      , MonadFix m
>      )
>   => m ()
> exampleUsage = do
>   let

Construct the ``<svg>`` element.

>     dSvgProps = pure $ S.SVG_El
>       (S.Width 400)
>       (S.Height 300)

Create a normal ``Map`` of HTML attributes to apply to the shape

>     attrs = mempty
>       & ix "id" .~ "svg1"
>       & ix "class" .~ "blue no-yellow"

Build our first ``<rect>``.

>     dRect1 = pure $ S.SVG_Rect
>       ((40.0 :: Float) ^. from S._PosX)
>       ((40.0 :: Float) ^. from S._PosY)
>       (S.Width 50.0)
>       (S.Height 50.0)
>       Nothing
>       Nothing

This is the same as writing: <rect x="40" y="40" height="50" width="50">.

SVG_Rect ((40.0 :: Float) ^. posX) ((40.0 :: Float) ^. posY) (Width 50.0) (Height 50.0) Nothing Nothing

We can use lenses to modify the properties of our shape.

>     shiftRect = 
>       fmap (S.svg_rect_pos_x . S._PosX +~ (3.0 :: Float))

We can also define a ``<rect>`` with corner radius.

>     dRect3 = pure $ S.SVG_Rect
>       ((20.0 :: Float) ^. from S._PosX)
>       ((20.0 :: Float) ^. from S._PosY)
>       (S.Width 30.0)
>       (S.Height 30.0)
>       ((15.0 :: Float) ^? from S._CornerRadiusX)
>       ((15.0 :: Float) ^? from S._CornerRadiusY)

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
>     S.svgRectDyn_ dRect1 >> S.svgRectDyn dRect3 ( pure $ S.Animate =: dAnim )
>
>   pure ()