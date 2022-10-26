{pkgs, ...}: {
  xdg.dataFile."dict/words".source = pkgs.callPackage (
    {
      runCommand,
      hunspellDicts,
    }:
      runCommand "words" {} ''
        for file in "${hunspellDicts.en-us-large}/share/hunspell/en_US.dic" \
                    "${hunspellDicts.en-gb-large}/share/hunspell/en_GB.dic"
        do
          cut -d/ -f1 $file \
            | grep -vE "^[[:digit:]]" \
            | grep -vE "^[[:upper:]]+$" \
            >> temp
        done
        cat temp | sort | uniq > $out
      ''
  ) {};
}
