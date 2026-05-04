{
  delib,
  pkgs,
  homeconfig,
  ...
}:
delib.module {
  name = "zsh";

  options =
    with delib;
    moduleOptions {
      enable = boolOption true;

      plugins = listOfOption attrs [
        {
          name = "nix-shell";
          src = pkgs.customZshPlugins.zsh-nix-shell;
        }
        {
          name = "fast-syntax-highlighting";
          src = pkgs.customZshPlugins.zsh-fast-syntax-highlighting;
        }
      ];
    };

  home.ifEnabled =
    { cfg, ... }:
    let
      dotDir = "${homeconfig.xdg.configHome}/zsh";
    in
    {
      programs.zsh = {
        enable = true;
        inherit dotDir;
        autosuggestion.enable = true;
        enableCompletion = true;
        defaultKeymap = "emacs";
        plugins = cfg.plugins;
        sessionVariables = {
          "DIRSTACKSIZE" = "20";
        };

        shellAliases = {
          ".." = "cd ..";
          "..." = "cd ../..";
          "rm" = "rm -i";
          "start" = "systemctl --user start";
          "stop" = "systemctl --user stop";
          "enable" = "systemctl --user enable";
          "disable" = "systemctl --user disable";
          "reload" = "systemctl --user daemon-reload";
          "status" = "systemctl --user --full status";
          "restart" = "systemctl --user restart";
          "ssh-ignore" = "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null";
        };

        initContent = ''
          setopt auto_cd
          setopt cdable_vars
          setopt auto_name_dirs
          setopt auto_pushd
          setopt pushd_ignore_dups
          setopt pushdminus

          export EDITOR=emacsclient

          # Use gpg-agent as ssh-agent.
          gpg-connect-agent /bye
          export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
        '';
      };

      home.packages = with pkgs; [
        zsh
        nix-zsh-completions
      ];
    };

  nixos.ifEnabled = {
    programs.zsh.enable = true;
    users.defaultUserShell = pkgs.zsh;
  };
}
