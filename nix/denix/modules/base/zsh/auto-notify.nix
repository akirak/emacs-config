{ delib, pkgs, ... }:
delib.module {
  name = "zsh.auto-notify";

  options =
    with delib;
    moduleOptions {
      enable = boolOption true;
    };

  home.ifEnabled = {
    programs.zsh = {
      plugins = [
        {
          name = "auto-notify";
          src = pkgs.customZshPlugins.zsh-auto-notify;
        }
      ];
      sessionVariables = {
        # https://github.com/MichaelAquilina/zsh-auto-notify
        "AUTO_NOTIFY_THRESHOLD" = "60";
      };
      initContent = ''
        export AUTO_NOTIFY_IGNORE=("nix shell" "nix develop" "ssh" "agenix edit" "man" "less")
      '';
    };

    home.packages = [
      pkgs.libnotify
    ];
  };
}
