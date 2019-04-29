0.3.2.0

* Bumped reflex-platform 
* Added 'example' project
* Fix issue that prevented 'animate' attributes from rendering correctly
* Fix 'ellipse' 'cy' property
* Fix 'circle' 'radius' type and add an example to the Example file.
* Update the Example.lhs to work with latest changes
* Remove the shell.nix
* Update default.nix to use 'shellAware' function
* Update element types to more general 'Element' type
* Remove image from cabal file, add readme to extra-source-files

0.2.0.0

* Add "viewBox" property support for root SVG tag. This breaks existing SVG_El definitions.
* Expose the 'raw' svg drawing functions.
* Update default.nix to not try to generate haddocks on GHCJS build. Would cause the build to fail.
* Add a helper function to change the indexed type of a Pos.

0.1.1.0

* First version. Released on an unsuspecting world.
* Add import for SVG_Circle
* Update the props functions to correctly update the attribute map
