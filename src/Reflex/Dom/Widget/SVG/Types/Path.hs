{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Reflex.Dom.Widget.SVG.Types.Path where

import           Control.Lens                         (Prism', prism, ( # ),
                                                       (^.))

import           Data.List.NonEmpty                   (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                   as NE

import           Data.Semigroup                       (Semigroup (..), (<>))

import           Data.Text                            (Text)
import qualified Data.Text                            as Text

import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Pos      (Pos, X, Y, _PosX, _PosY)

-- | Do not consider any of this final or usable, it's a mess and needs work

data PathCommandType
  -- Normal line movements
  = MoveTo (Pos X) (Pos Y)
  | LineTo (Pos X) (Pos Y)
  | Horizontal (Pos X)
  | Vertical (Pos Y)

  | Bezier CurveTo

  | ClosePath

data CurveTo
  = SmthQuadBez (Pos X) (Pos Y)
  | QuadBez (Pos X) (Pos Y) (Pos X) (Pos Y)
  | SmthCubicBez (Pos X) (Pos Y) (Pos X) (Pos Y)
  | CubicBez (Pos X) (Pos Y) (Pos X) (Pos Y) (Pos X) (Pos Y)

data PathCommandRelativity
  = Relative
  | NotRelative

data PathCommand
  = PathComm PathCommandType PathCommandRelativity

newtype SVG_Path =
  D (NonEmpty PathCommand)
  deriving Semigroup

_MoveTo :: Prism' PathCommandType (Pos X, Pos Y)
_MoveTo = prism (uncurry MoveTo)
  (\case MoveTo x y -> Right (x, y)
         x          -> Left x)

_LineTo :: Prism' PathCommandType (Pos X, Pos Y)
_LineTo = prism (uncurry LineTo)
  (\case LineTo x y -> Right (x, y)
         x          -> Left x)

_Horizontal :: Prism' PathCommandType (Pos X)
_Horizontal = prism Horizontal
  (\case Horizontal y1 -> Right y1
         x             -> Left x)

_Vertical :: Prism' PathCommandType (Pos Y)
_Vertical = prism Vertical
  (\case Vertical y1 -> Right y1
         x           -> Left x)

_ClosePath :: Prism' PathCommandType ()
_ClosePath = prism (const ClosePath)
  (\case ClosePath -> Right ()
         x         -> Left x)

_PathComm :: Prism' PathCommand (PathCommandType, PathCommandRelativity)
_PathComm = prism (uncurry PathComm) (\(PathComm t r) -> Right (t, r))

notRelativePath, relativePath :: PathCommandType -> PathCommand
relativePath pc = _PathComm # (pc, Relative)
notRelativePath pc = _PathComm # (pc, NotRelative)

_D :: Prism' SVG_Path (NonEmpty PathCommand)
_D = prism D (\(D p) -> Right p)

_m :: Pos X -> Pos Y -> PathCommand
_m x y = relativePath $ _MoveTo # (x,y)

_M :: Pos X -> Pos Y -> PathCommand
_M x y = notRelativePath $ _MoveTo # (x,y)

_l :: Pos X -> Pos Y -> PathCommand
_l x y = relativePath $ _LineTo # (x,y)

_L :: Pos X -> Pos Y -> PathCommand
_L x y = notRelativePath $ _LineTo # (x,y)

_h :: Pos X -> PathCommand
_h = relativePath . (_Horizontal #)

_H :: Pos X -> PathCommand
_H = notRelativePath . (_Horizontal #)

_v :: Pos Y -> PathCommand
_v = relativePath . (_Vertical #)

_V :: Pos Y -> PathCommand
_V = notRelativePath . (_Vertical #)

_z :: PathCommand
_z = relativePath (_ClosePath # ())

_Z :: PathCommand
_Z = notRelativePath (_ClosePath # ())

relativeCommand :: Text -> PathCommandRelativity -> Text
relativeCommand c' Relative = Text.toLower c'
relativeCommand c' _        = c'

pathCommandToText
  :: PathCommandType
  -> PathCommandRelativity
  -> Text
pathCommandToText pc pcr =
  case pc of
    MoveTo x y   ->
      relativeCommand "M" pcr        <> x ^. wrappedToText <> "," <> y ^. wrappedToText
    LineTo x y   ->
      relativeCommand "L" pcr <> " " <> x ^. wrappedToText <> "," <> y ^. wrappedToText
    Horizontal x ->
      relativeCommand "H" pcr <> " " <> x ^. wrappedToText
    Vertical y   ->
      relativeCommand "V" pcr <> " " <> y ^. wrappedToText
    ClosePath    ->
      relativeCommand "Z" pcr

s :: SVG_Path
s = _D # NE.fromList
  [ _M (_PosX # 3.0) (_PosY # 4.0)
  , _L (_PosX # 10.0) (_PosY # 12.0)
  , _L (_PosX # 15.0) (_PosY # 30.0)
  , _z
  ]
