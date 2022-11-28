{pkgs, ...}: {
  xdg.dataFile."dict/words".source =
    (pkgs.fetchFromGitHub {
      owner = "dwyl";
      repo = "english-words";
      rev = "a77cb15f4f5beb59c15b945f2415328a6b33c3b0";
      sha256 = "12mj9yas7z48cjy0zpi7jz09svipasr62v786lj3wkdssy8wrbfj";
    })
    + "/words_alpha.txt";
}
