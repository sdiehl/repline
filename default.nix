{ nixpkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

let haskell = nixpkgs.pkgs.haskell;
    packages = haskell.packages.${compiler};
in
packages.callPackage ./repline.nix {
  # Override the default version of haskeline as 0.8.0.0 is not the default
  # version as of NixOS 20.03 (and its test suite fails to runs properly)
  haskeline = haskell.lib.dontCheck packages.haskeline_0_8_0_0;
}
