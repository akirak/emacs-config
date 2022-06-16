{
  stdenv,
  fetchzip,
}:
stdenv.mkDerivation {
  name = "shippori-mincho-otf";
  src = fetchzip {
    url = "https://fontdasu.com/download/shippori3.zip";
    sha256 = "0bi01dxy9s6xl9fizcwixqkv81h9cgcwqsr94vd8n8w21n9app6v";
    stripRoot = false;
  };

  dontBuild = true;

  installPhase = ''
    fontDir=$out/share/fonts/truetype
    mkdir -p $fontDir
    cp *.otf $fontDir
  '';
}
