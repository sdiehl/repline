{ mkDerivation, base, containers, exceptions, haskeline, mtl
, process, stdenv
}:
mkDerivation {
  pname = "repline";
  version = "0.4.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers exceptions haskeline mtl process
  ];
  testHaskellDepends = [ base containers mtl process ];
  homepage = "https://github.com/sdiehl/repline";
  description = "Haskeline wrapper for GHCi-like REPL interfaces";
  license = stdenv.lib.licenses.mit;
}
