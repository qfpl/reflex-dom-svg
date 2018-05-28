{ mkDerivation, base, containers, lens, reflex, reflex-dom-core
, safe, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-svg";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers lens reflex reflex-dom-core safe text
  ];
  description = "Reflex functions for SVG elements";
  license = stdenv.lib.licenses.bsd3;
}
