{pkgs, ...}: {
  home.packages = with pkgs; [
    gh
    pre-commit
    nixpkgs-fmt
    alejandra
    deadnix
    squasher
    drawio

    hunspellDicts.en_US
  ];

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;

    # Omit the global user identity
    # userEmail = "akira.komamura@gmail.com";
    # userName = "Akira Komamura";

    extraConfig = {
      github.user = "akirak";

      pull.rebase = false;

      "url \"git@github.com:\"".pushInsteadOf = "https://github.com/";

      core.autocrlf = "input";

      # Only on WSL
      # core.fileMode = false;

      # Increase the size of post buffers to prevent hung ups of git-push.
      # https://stackoverflow.com/questions/6842687/the-remote-end-hung-up-unexpectedly-while-git-cloning#6849424
      http.postBuffer = "524288000";

      # Use a global config to track configuration files for alternative identities
      include.path = "~/.gitconfig";

      "includeIf \"hasconfig:remote.*.url:git@git.sr.ht:~akirak/**\"".path = "${./git/default-identity}";

      "includeIf \"hasconfig:remote.*.url:https://github.com/akirak/**\"".path = "${./git/default-identity}";

      "includeIf \"hasconfig:remote.*.url:git@github.com:akirak/**\"".path = "${./git/default-identity}";

      "includeIf \"hasconfig:remote.*.url:https://github.com/emacs-twist/**\"".path = "${./git/default-identity}";

      "includeIf \"hasconfig:remote.*.url:git@github.com:emacs-twist/**\"".path = "${./git/default-identity}";
    };

    ignores = [
      ".direnv"
      "#*"
    ];
  };
}
