-- | Ugh, this is all a work in progress.
module Reflex.Dom.Widget.SVG.Path where

import Reflex.Dom.Widget.SVG.Types (Pos, X, Y)

-- | Do not consider any of this final or usable, it's a mess and needs work

data PathCommandType
  = MoveTo (Pos X) (Pos Y)
  | LineTo (Pos X) (Pos Y)
  | Horizontal Float
  | Vertical Float

data PathCommandRelativity
  = Relative
  | NotRelative

data PathCommand
  = PathPiece PathCommandType ( Maybe PathCommandRelativity )
  | ClosePath

newtype SVG_Path = SVG_Path (NonEmpty PathCommand)
