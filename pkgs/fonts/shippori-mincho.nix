{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation {
  name = "shippori-mincho-otf";
  src = fetchzip {
    url = "https://fontdasu.com/download/shippori3.zip";
    sha256 = "0zh1kj905hw8jmx5kmdsq03y0r4ssad7v36ayh8wmnz45wdfssy0";
    stripRoot = false;
  };

  dontBuild = true;

  installPhase = ''
    fontDir=$out/share/fonts/truetype
    mkdir -p $fontDir
    cp *.otf $fontDir
  '';
}
