{ nixpkgs ? import ./nixpkgs.nix {}
, compiler ? "ghc865"
}:

(import ./default.nix { inherit nixpkgs compiler; }).env
