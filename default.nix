
with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "graphdb-env";

  buildInputs = [
    (idrisPackages.with-packages (with idrisPackages; [
      #-- put idris packages here
      effects aatree
    ]))
  ];

  #-- alias idris to set path and load packages
  shellHook = ''
    alias idris='idris -i./src -p effects -p aatree'
  '';

}



