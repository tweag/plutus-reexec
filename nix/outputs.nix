{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix { inherit inputs pkgs lib project utils ghc; };

  packages = {};
  
  devShells = rec {
    default = ghc966; 
    ghc966 = mkShell "ghc966"; 
  };

  projectFlake = project.flake {};
in
{
  inherit packages;
  inherit devShells;
}
