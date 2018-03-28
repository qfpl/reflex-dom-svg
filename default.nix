{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghc"
} :
let
  adjust-for-ghcjs = drv: {
    doHaddock = false;
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  pkgs = reflex-platform.nixpkgs.pkgs;
  drv = pkgs.haskell.lib.overrideCabal (reflex-platform.${compiler}.callPackage ./reflex-dom-svg.nix {}) adjust;
in
  drv
