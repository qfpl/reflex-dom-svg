{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler ? "ghc"
} :
let
  drv = reflex-platform.${compiler}.callPackage ./reflex-dom-svg.nix {};
in
  reflex-platform.nixpkgs.haskell.lib.shellAware drv
