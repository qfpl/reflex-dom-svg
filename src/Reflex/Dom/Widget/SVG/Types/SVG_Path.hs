{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Reflex.Dom.Widget.SVG.Types.SVG_Path
  ( PathCommandType (..)
  , PathCommandRelativity (..)
  , SVG_Path (..)
  , P (..)
  , _SmthQuadBez
  , _QuadBez
  , _SmthCubicBez
  , _CubicBez
  , _MoveTo
  , _LineTo
  , _Horizontal
  , _Vertical
  , _ClosePath
  , _PathComm
  , _m
  , _M
  , _l
  , _L
  , _h
  , _H
  , _v
  , _V
  , _z
  , _Z
  , makePathProps
  , pathCommandToText
  ) where

import           Control.Lens                         (Prism', Rewrapped,
                                                       Wrapped (..), iso, prism,
                                                       to, ( # ), (^.), (^..),
                                                       _1, _2, _Wrapped)

import           Data.Map                             (Map)

import           Data.List.NonEmpty                   (NonEmpty)

import           Data.Semigroup                       (Semigroup (..), (<>))

import           Data.Text                            (Text)
import qualified Data.Text                            as Text

import           Reflex.Dom                           ((=:))

import           Reflex.Dom.Widget.SVG.Types.Internal (wrappedToText)
import           Reflex.Dom.Widget.SVG.Types.Pos      (Pos, X, Y)

-- | Do not consider any of this final or usable, it's a mess and needs work

newtype P =
  P (Pos X, Pos Y)

instance P ~ t => Rewrapped P t

instance Wrapped P where
  type Unwrapped P = (Pos X, Pos Y)
  _Wrapped' = iso (\(P x) -> x) P

data PathCommandType
  -- Normal line movements
  = MoveTo P
  | LineTo P
  | Horizontal (Pos X)
  | Vertical (Pos Y)
  -- Curves!
  | SmthQuadBez P
  | QuadBez P P
  | SmthCubicBez P P
  | CubicBez P P P

  | ClosePath

data PathCommandRelativity
  = Relative
  | NotRelative

data PathCommand
  = PathComm PathCommandType PathCommandRelativity

newtype SVG_Path =
  D (NonEmpty PathCommand)
  deriving Semigroup

instance SVG_Path ~ t => Rewrapped SVG_Path t

instance Wrapped SVG_Path where
  type Unwrapped SVG_Path = (NonEmpty PathCommand)
  _Wrapped' = iso (\(D x) -> x) D

_SmthQuadBez :: Prism' PathCommandType P
_SmthQuadBez = prism SmthQuadBez
  (\case SmthQuadBez x1y1 -> Right x1y1
         x                -> Left x
  )

_QuadBez :: Prism' PathCommandType (P, P)
_QuadBez = prism (uncurry QuadBez)
  (\case QuadBez x1y1 x2y2 -> Right (x1y1, x2y2)
         x                 -> Left x
  )

_SmthCubicBez :: Prism' PathCommandType (P, P)
_SmthCubicBez = prism (uncurry SmthCubicBez)
  (\case SmthCubicBez x1y1 x2y2 -> Right (x1y1, x2y2)
         x                        -> Left x
  )

_CubicBez :: Prism' PathCommandType (P, P, P)
_CubicBez = prism (\(x1y1,x2y2,x3y3) -> CubicBez x1y1 x2y2 x3y3)
  (\case CubicBez x1y1 x2y2 x3y3 -> Right (x1y1, x2y2, x3y3)
         x                          -> Left x
  )

_MoveTo :: Prism' PathCommandType P
_MoveTo = prism MoveTo
  (\case MoveTo xy -> Right xy
         x         -> Left x)

_LineTo :: Prism' PathCommandType P
_LineTo = prism LineTo
  (\case LineTo xy -> Right xy
         x         -> Left x)

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

_m,_M,_l,_L :: Pos X -> Pos Y -> PathCommand
_m x y = relativePath $ _MoveTo . _Wrapped # (x,y)
_M x y = notRelativePath $ _MoveTo . _Wrapped # (x,y)
_l x y = relativePath $ _LineTo . _Wrapped # (x,y)
_L x y = notRelativePath $ _LineTo . _Wrapped # (x,y)

_h,_H :: Pos X -> PathCommand
_h = relativePath . (_Horizontal #)
_H = notRelativePath . (_Horizontal #)

_v,_V :: Pos Y -> PathCommand
_v = relativePath . (_Vertical #)
_V = notRelativePath . (_Vertical #)

_z,_Z :: PathCommand
_z = relativePath (_ClosePath # ())
_Z = notRelativePath (_ClosePath # ())

pathCommandToText
  :: PathCommandType
  -> PathCommandRelativity
  -> Text
pathCommandToText pc pcr =
  let
    rComm c' Relative = Text.toLower c'
    rComm c' _        = c'

    pt2 xy =
      let
        ptx = xy ^. _Wrapped . _1 . wrappedToText
        pty = xy ^. _Wrapped . _2 . wrappedToText
      in
        ptx <> "," <> pty
  in
    case pc of
      MoveTo xy    -> rComm "M" pcr <> pt2 xy
      LineTo xy    -> rComm "L" pcr <> " " <> pt2 xy
      Horizontal x -> rComm "H" pcr <> " " <> x ^. wrappedToText
      Vertical y   -> rComm "V" pcr <> " " <> y ^. wrappedToText

      SmthQuadBez xy              -> rComm "T" pcr <> pt2 xy
      QuadBez cpxcpy1 xy          -> rComm "Q" pcr <> pt2 cpxcpy1 <> pt2 xy
      SmthCubicBez cpxcpy2 xy     -> rComm "S" pcr <> pt2 cpxcpy2 <> pt2 xy
      CubicBez cpxcpy1 cpxcpy2 xy -> rComm "C" pcr <> pt2 cpxcpy1 <> pt2 cpxcpy2 <> pt2 xy

      ClosePath -> rComm "Z" pcr

makePathProps
  :: SVG_Path
  -> Map Text Text
makePathProps p =
  let
    pcs = p ^.. _Wrapped . traverse . _PathComm . to (uncurry pathCommandToText)
  in
    "d" =: Text.unwords pcs

