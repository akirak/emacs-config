{ stdenv, fetchzip }:
stdenv.mkDerivation {
  name = "jetbrains-mono-nerdfont";

  src = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/JetBrainsMono.zip";
    sha256 = "1dzdys75r00i86qxm5b4ibbjbgimpmj7d35l7nd4y311mhq4ki13";
    stripRoot = false;
  };

  dontBuild = true;

  installPhase = ''
    fontDir=$out/share/fonts/truetype
    mkdir -p $fontDir
    cp *.ttf $fontDir
  '';
}
