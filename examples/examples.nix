{ mkDerivation, base, containers, ghcjs-dom, jsaddle, lens, mtl
, random, reflex, reflex-dom, reflex-dom-svg, stdenv, text, time
}:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers ghcjs-dom jsaddle lens mtl random reflex reflex-dom
    reflex-dom-svg text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
