with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  nativeBuildInputs = [
    automake
    autoconf
    pkg-config
    gcc 
    swiProlog
  ];
  buildInputs = [
  ];
}

