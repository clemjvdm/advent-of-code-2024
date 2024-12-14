{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      regex-compat
      regex-tdfa
    ]))
  ];
}
