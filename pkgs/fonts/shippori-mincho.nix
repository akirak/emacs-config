{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation {
  name = "shippori-mincho-otf";
  src = fetchzip {
    url = "https://fontdasu.com/download/shippori3.zip";
    sha256 = "14k9k30m29wbsrdcb0ayws6mxk3pabyml4kakig90q77f2niwy2c";
    stripRoot = false;
  };

  dontBuild = true;

  installPhase = ''
    fontDir=$out/share/fonts/truetype
    mkdir -p $fontDir
    cp *.otf $fontDir
  '';
}
