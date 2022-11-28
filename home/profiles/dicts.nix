{pkgs, ...}: {
  xdg.dataFile."dict/wordnet.db".source =
    pkgs.wordnet-sqlite;
}
