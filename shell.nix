{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    stack
  ];

  shellHook = ''
    echo "Haskell development environment loaded!"
    echo "Available tools:"
    echo " - GHC $(ghc --version)"
    echo " - Stack $(stack --version)"
  '';
}
