> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Example where

> import Control.Lens ((^.))
> import Control.Monad.Fix (MonadFix)

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

Build our first ``<rect>``.
>     dRect1 = pure $ S.SVG_Rect
>       ((40.0 :: Float) ^. S.posX)
>       ((40.0 :: Float) ^. S.posY)
>       (S.Width 50.0)
>       (S.Height 50.0)
>       Nothing
>       Nothing
This is the same as writing: <rect x="40" y="40" height="50" width="50">.

We can also define a ``<rect>`` with corner radius.
>     dRect2 = pure $ S.SVG_Rect
>       ((20.0 :: Float) ^. S.posX)
>       ((20.0 :: Float) ^. S.posY)
>       (S.Width 30.0)
>       (S.Height 30.0)
>       (15.0 ^? S.cornerRadiusX)
>       (15.0 ^? S.cornerRadiusY)
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
>   _ <- S.svg_ dSvgProps $
>     S.svgRectDyn_ dRect1 >>
>     S.svgRectDyn dRect2 ( pure $ S.Animate =: dAnim )
>
>   pure ()