{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}: {
  source-han-serif =
    stdenvNoCC.mkDerivation
    {
      src = fetchFromGitHub {
        owner = "adobe-fonts";
        repo = "source-han-serif";
        rev = "f4726a2ec36169abd02a6d8abe67c8ff0236f6d8";
        sha256 = "0zc1r7zph62qmvzxqfflsprazjf6x1qnwc2ma27kyzh6v36gaykw";
      };

      installPhase = ''
        # We copy in reverse preference order -- unhinted first, then
        # hinted -- to get the "best" version of each font while
        # maintaining maximum coverage.
        #
        # TODO: install OpenType, variable versions?
        local out_ttf=$out/share/fonts/truetype/noto
        install -m444 -Dt $out_ttf phaseIII_only/unhinted/ttf/*/*-${weights}.ttf
        install -m444 -Dt $out_ttf phaseIII_only/hinted/ttf/*/*-${weights}.ttf
        install -m444 -Dt $out_ttf unhinted/*/*-${weights}.ttf
        install -m444 -Dt $out_ttf hinted/*/*-${weights}.ttf
      '';

      meta = with lib; {
        homepage = "https://github.com/adobe-fonts/source-han-serif";
        license = licenses.ofl;
        platforms = platforms.all;
      };
    };
}
