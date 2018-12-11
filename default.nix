with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "graphdb-env";

  buildInputs = [
    (idrisPackages.with-packages (with idrisPackages; [
      #-- put idris packages here
      effects aatree
    ]))
  ];

  #-- alias idris to load all packages
  shellHook = ''
    alias idris='idris -p effects -p aatree'
  '';

}



