{ pkgs ? import <nixpkgs> { } }:

let
  R-with-my-pkgs = pkgs.rWrapper.override {
    packages = [ pkgs.rPackages.matlib pkgs.rPackages.numDeriv pkgs.curl ];
  };
in pkgs.mkShell { buildInputs = [ R-with-my-pkgs ]; }
