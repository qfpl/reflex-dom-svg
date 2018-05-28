{ mkDerivation, base, containers, lens, reflex
, reflex-dom-core, safe, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-svg";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers lens reflex reflex-dom-core safe text
  ];
  license = stdenv.lib.licenses.bsd3;
}
