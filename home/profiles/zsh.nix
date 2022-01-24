{ inputs, config, pkgs, ... }:
{
  home.packages = with pkgs; [
    zsh
    nix-zsh-completions
    fzy
  ];

  programs.zsh =
    {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      dotDir = ".config/zsh";
      defaultKeymap = "emacs";
      history = {
        expireDuplicatesFirst = true;
        save = 5000;
        share = true;
        size = 5000;
      };
      plugins = [
        {
          name = "zsh-history-substring-search";
          src = pkgs.zsh-history-substring-search;
        }
        {
          name = "enhancd";
          src = pkgs.zsh-enhancd;
        }
        {
          name = "fzy";
          src = pkgs.zsh-fzy;
        }
        {
          name = "nix-shell";
          src = pkgs.zsh-nix-shell;
        }
        {
          name = "fast-syntax-highlighting";
          src = pkgs.zsh-fast-syntax-highlighting;
        }
      ];
      sessionVariables = {
        "DIRSTACKSIZE" = "20";
        "NIX_BUILD_SHELL" = "zsh";
        # "VAGRANT_WSL_WINDOWS_ACCESS" = "1";
        # Set locale archives
        # https://github.com/NixOS/nixpkgs/issues/38991
        "LOCALE_ARCHIVE_2_11" = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        "LOCALE_ARCHIVE_2_27" = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        LANG = "en_GB.UTF-8";
        LANGUAGE = "en_US:zh_CN:zh_TW:en";
        LC_ALL = "C";
        LC_CTYPE = "en_GB";
        LC_MESSAGES = "en_GB";
        # Use ISO 8601 (YYYY-MM-DD) date format
        LC_TIME = "en_DK.UTF-8";

        # STARDICT_DATA_DIR = "${config.home.homeDirectory}/.nix-profile/share/stardict";
        # "GOPATH" = "${builtins.getEnv "HOME"}/misc/go";
      };
      initExtra = ''
        setopt auto_cd
        setopt cdable_vars
        setopt auto_name_dirs
        setopt auto_pushd
        setopt pushd_ignore_dups
        setopt pushdminus

        # Configuration for zsh-fzy plugin https://github.com/aperezdc/zsh-fzy
        bindkey '\eq' fzy-proc-widget
        bindkey '\ew' fzy-cd-widget
        bindkey '\ee' fzy-file-widget
        bindkey '\er' fzy-history-widget
        zstyle :fzy:file command fd -t f
        zstyle :fzy:cd command fd -t d

        # Support directory tracking on emacs-libvterm.
        # https://github.com/akermu/emacs-libvterm#directory-tracking
        function chpwd() {
            print -Pn "\e]51;A$(pwd)\e\\";
        }

        export NIX_BUILD_SHELL=bash

        # Use gpg-agent as ssh-agent.
        gpg-connect-agent /bye
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

        export STARSHIP_CONFIG=${
          pkgs.writeText "starship.toml"
            (pkgs.lib.fileContents ../../dotfiles/starship.toml)
        }

        eval "$(${pkgs.starship}/bin/starship init zsh)"

        # if [[ -f ~/.asdf/asdf.sh ]]; then
        #    source ~/.asdf/asdf.sh
        # fi
      '';
      shellAliases = {
        ".." = "cd ..";
        "..." = "cd ../..";
        # "nvfetcher" = "nix run github:berberman/nvfetcher";
        # Drop these in favour of exa
        # "ls" = "ls --color=auto";
        # "la" = "ls -a";
        # "ll" = "ls -l";
        "rm" = "rm -i";
        "j" = "journalctl -xe";
        "start" = "systemctl --user start";
        "stop" = "systemctl --user stop";
        "enable" = "systemctl --user enable";
        "disable" = "systemctl --user disable";
        "reload" = "systemctl --user daemon-reload";
        "status" = "systemctl --user --full status";
        "restart" = "systemctl --user restart";
        "list-units" = "systemctl --user list-units";
        "list-unit-files" = "systemctl --user list-unit-files";
        "reset" = "systemctl --user reset-failed";
        "nsearch" = "nix search --no-update-lock-file nixpkgs";
      };

      #       profileExtra = ''
      # emulate sh
      # if [ -f /etc/profile ] && [ ! -v __ETC_PROFILE_DONE ]; then
      #   . /etc/profile
      # fi
      # if [ -f ~/.profile ]; then
      #   . ~/.profile
      # fi
      # emulate zsh

      # if [[ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]]; then
      #   source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
      # fi
      # '';
    };

}
