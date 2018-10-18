{ mkDerivation, base, containers, dependent-map, dependent-sum
, fetchgit, mtl, primitive, ref-tf, reflex, stdenv, stm, time
}:
mkDerivation {
  pname = "reflex-basic-host";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/dalaing/reflex-basic-host";
    sha256 = "1cw9lbgl10m6w59fjs4xhjql6p8vvr7acf4wsv2bh5xd8p5svbdm";
    rev = "cf7efd48855e30c0940897e12ac694b3617d4e56";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-map dependent-sum mtl primitive ref-tf reflex stm
  ];
  executableHaskellDepends = [ base containers mtl reflex time ];
  license = stdenv.lib.licenses.bsd3;
}
