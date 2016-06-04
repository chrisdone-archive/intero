{ mkDerivation, array, base, bytestring, containers, directory
, filepath, ghc, ghc-paths, haskeline, hspec, process, regex-compat
, stdenv, syb, temporary, time, transformers, unix
}:
mkDerivation {
  pname = "intero";
  version = "0.1.13";
  src = builtins.filterSource
          (path: type: let baseName = baseNameOf path; in with stdenv.lib;
           !(hasSuffix "~" baseName || hasPrefix ".#" baseName || (type == "directory" && baseName == "elisp")))
          ./.;
  isLibrary = false;
  isExecutable = true;
  prePatch = ''
    sed -i 's|elisp/*.el||' ./intero.cabal
    sed -i 's|data-files:||' ./intero.cabal
  '';
  executableHaskellDepends = [
    array base bytestring containers directory filepath ghc ghc-paths
    haskeline process syb time transformers unix
  ];
  testHaskellDepends = [
    base directory hspec process regex-compat temporary transformers
  ];
  doCheck = false;
  homepage = "https://github.com/commercialhaskell/intero";
  description = "Complete interactive development program for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
