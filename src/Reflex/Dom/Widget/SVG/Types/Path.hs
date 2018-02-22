{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Reflex.Dom.Widget.SVG.Types.Path where

import           Control.Lens                         (Prism', prism, view,
                                                       ( # ))

import           Data.List.NonEmpty                   (NonEmpty)
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
  -- Curves!
  | SmthQuadBez (Pos X) (Pos Y)
  | QuadBez (Pos X) (Pos Y) (Pos X) (Pos Y)
  | SmthCubicBez (Pos X) (Pos Y) (Pos X) (Pos Y)
  | CubicBez (Pos X) (Pos Y) (Pos X) (Pos Y) (Pos X) (Pos Y)

  | ClosePath

data PathCommandRelativity
  = Relative
  | NotRelative

data PathCommand
  = PathComm PathCommandType PathCommandRelativity

newtype SVG_Path =
  D (NonEmpty PathCommand)
  deriving Semigroup

_SmthQuadBez :: Prism' PathCommandType (Pos X, Pos Y)
_SmthQuadBez = prism (uncurry SmthQuadBez)
  (\case SmthQuadBez x y -> Right (x,y)
         x               -> Left x
  )

_QuadBez :: Prism' PathCommandType (Pos X, Pos Y, Pos X, Pos Y)
_QuadBez = prism (\(x1,y1,x2,y2) -> QuadBez x1 y1 x2 y2)
  (\case QuadBez x1 y1 x2 y2 -> Right (x1, y1, x2, y2)
         x                   -> Left x
  )

_SmthCubicBez :: Prism' PathCommandType (Pos X, Pos Y, Pos X, Pos Y)
_SmthCubicBez = prism (\(x1,y1,x2,y2) -> SmthCubicBez x1 y1 x2 y2)
  (\case SmthCubicBez x1 y1 x2 y2 -> Right (x1, y1, x2, y2)
         x                        -> Left x
  )

_CubicBez :: Prism' PathCommandType (Pos X, Pos Y, Pos X, Pos Y, Pos X, Pos Y)
_CubicBez = prism (\(x1,y1,x2,y2,x3,y3) -> CubicBez x1 y1 x2 y2 x3 y3)
  (\case CubicBez x1 y1 x2 y2 x3 y3 -> Right (x1, y1, x2, y2, x3, y3)
         x                          -> Left x
  )

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

rComm :: Text -> PathCommandRelativity -> Text
rComm c' Relative = Text.toLower c'
rComm c' _        = c'

pathCommandToText
  :: PathCommandType
  -> PathCommandRelativity
  -> Text
pathCommandToText pc pcr =
  let
    pt = view wrappedToText
    pt2 x y = pt x <> "," <> pt y
  in
    case pc of
      MoveTo x y ->
        rComm "M" pcr        <> pt2 x y
      LineTo x y ->
        rComm "L" pcr <> " " <> pt2 x y
      Horizontal x ->
        rComm "H" pcr <> " " <> pt x
      Vertical y ->
        rComm "V" pcr <> " " <> pt y

      SmthQuadBez x y ->
        rComm "T" pcr <> pt2 x y
      QuadBez cpx1 cpy1 x y ->
        rComm "Q" pcr <> pt2 cpx1 cpy1 <> pt2 x y
      SmthCubicBez cpx2 cpy2 x y ->
        rComm "S" pcr <> pt2 cpx2 cpy2 <> pt2 x y
      CubicBez cpx1 cpy1 cpx2 cpy2 x y ->
        rComm "C" pcr <> pt2 cpx1 cpy1 <> pt2 cpx2 cpy2 <> pt2 x y

      ClosePath ->
        rComm "Z" pcr

s :: SVG_Path
s = _D # NE.fromList
  [ _M (_PosX # 3.0) (_PosY # 4.0)
  , _L (_PosX # 10.0) (_PosY # 12.0)
  , _L (_PosX # 15.0) (_PosY # 30.0)
  , _z
  ]

