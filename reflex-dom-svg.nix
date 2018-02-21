{ mkDerivation, base, containers, lens, reflex
, reflex-dom, safe, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-svg";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers lens reflex reflex-dom safe text
  ];
  license = stdenv.lib.licenses.bsd3;
}
