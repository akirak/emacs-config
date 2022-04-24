{pkgs, ...}: {
  home.packages = with pkgs; [
    gh
    pre-commit
    nixpkgs-fmt
    alejandra
    deadnix
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
    };

    ignores = [
      ".direnv"
      "#*"
    ];
  };
}
