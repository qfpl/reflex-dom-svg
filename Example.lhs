> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Example where

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
>     dSvgProps = pure $ S.SVG_El
>       (S.Width 400)
>       (S.Height 300)
>
>     dRect1 = pure $ S.SVG_Rect
>       (S.Pos 40.0)
>       (S.Pos 40.0)
>       (S.Width 50.0)
>       (S.Height 50.0)
>       Nothing
>       Nothing
>
>     dRect2 = pure $ S.SVG_Rect
>       (S.Pos 20.0)
>       (S.Pos 20.0)
>       (S.Width 30.0)
>       (S.Height 30.0)
>       Nothing
>       Nothing
>
>     dAnim = S.makeAnimateProps $ S.SVG_Animate
>       ( S.AttributeName "x" )
>       ( S.AnimFrom 10 )
>       ( S.AnimTo 100 )
>       ( S.AnimDur "3s" )
>       ( S.RepeatCount "indefinite" )
>
>   _ <- S.svg_ dSvgProps $ do
>     _ <- S.svgRectDyn_ dRect1
>     S.svgRectDyn dRect2 ( pure $ S.Animate =: dAnim )
>
>   pure ()
