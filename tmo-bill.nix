{ mkDerivation, base, lib
, nixpkgs ? import <nixpkgs> {} }:
let
  report-parser-github = nixpkgs.fetchFromGitHub {
    owner = "p12nGH";
    repo = "report-parser";
    rev = "476c611758dd1e7d9bef7e9e30bbe240479d42c0";
    sha256 = "01g6yaax52qcck2hw5xpi56rz0parbbwfgrd3fl6nq5s0shll7yp";
  };
  report-parser = nixpkgs.haskellPackages.callPackage "${report-parser-github}/report-parser.nix" {};
in
  mkDerivation {
    pname = "tmo-bill";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = [ base report-parser ];
    description = "Utility for parsing T-Mobile PDF bills";
    license = lib.licenses.bsd3;
  }
