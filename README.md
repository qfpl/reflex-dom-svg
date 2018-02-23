<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

# Reflex Dom SVG Helper

A helper library for working with SVG and creating dynamic SVG images using
[reflex-dom](https://github.com/reflex-frp/reflex-dom). This library provides
a pile of types and some helper functions to make the process of creating and
managing your SVG elements, more type safe and a bit easier.

It currently supports the following SVG elements/properties:

* rect
* circle
* polygon
* polyline
* ellipse
* line
* animate
* path (included is an API for building type safe paths)

The SVG elements are designed to be created inside a SVG root element:

```haskell
let dSVGEl = pure $ SVG_El (Width 600.0) (Height 400)
a <- svg_ dSVGEl $ do
       ... -- other SVG elements go in here
```

Each of the SVG elements this library supports has a properties type
associated with it, similar to how the ``TextInput`` and other widget types
are configured in the core ``reflex-dom`` library.

```haskell
data SVG_Rect = SVG_Rect
  { _svg_rect_pos_x          :: Pos X
  , _svg_rect_pos_y          :: Pos Y
  , _svg_rect_width          :: Width
  , _svg_rect_height         :: Height
  , _svg_rect_cornerRadius_x :: Maybe (CornerRadius X)
  , _svg_rect_cornerRadius_y :: Maybe (CornerRadius Y)
  }
```

The respective module contains a function to take these properties and
convert them into the ``Map Text Text`` format that is expected by
``reflex-dom``. The functions in this module take care of this for you, and
allow you to add any other attributes as you require. You are able to manage
the properties of an SVG element as either a static input or a ``Dynamic``.

Additionally there is a (still work-in-progress) API for type safe ``<path>``
construction in the ``Reflex.Dom.Widget.SVG.Types.SVG_Path`` module. The
```SVG_Path`` type is a ``newtype`` over a ``NonEmpty PathCommand``, which
prevents you from having an empty list of Path instructions. As a minimal
example of using the API as it currently is:

```haskell
-- Drawing a simple rectangle using a <path>
r :: SVG_Path
r = _Wrapped # NonEmpty.fromList
  [ _M (_PosX # 10.0) (_PosY # 10.0)
  , _H (_PosX # 90.0)
  , _V (_PosY # 90.0)
  , _H (_PosX # 10.0)
  , _L (_PosX # 10.0) (_PosY # 10.0)
  ]
```

The functions ``_M``, ``_H``, ``_L``, and ``_V``, all correspond to their
``<path>`` commands from the [MDN Documentation on
Paths](https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths).
These are the non-relative variants of those commands, there are relative
versions as well, which are just lower-case named functions of the same type:
``_m``, ``_h``, and so on.