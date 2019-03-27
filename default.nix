
with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "graphdb-env";

  buildInputs = [
    (idrisPackages.with-packages (with idrisPackages; [
      #-- put idris packages here
      effects aatree lightyear testing
    ]))
  ];

  #-- alias idris to set path and load packages
  shellHook = ''
    idrisHook() { idris -i ./src -i ./test -p effects -p aatree -p lightyear -p testing "$@"; }
    alias idr=idrisHook
  '';

}



