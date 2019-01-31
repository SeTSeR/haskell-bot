{ mkDerivation, base, hpack, stdenv, telegram-api }:
mkDerivation {
  pname = "haskell-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base telegram-api ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base telegram-api ];
  testHaskellDepends = [ base telegram-api ];
  preConfigure = "hpack";
  homepage = "https://github.com/SeTSeR/haskell-bot#readme";
  license = stdenv.lib.licenses.bsd3;
}
