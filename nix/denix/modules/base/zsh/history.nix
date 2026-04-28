{
  delib,
  pkgs,
  ...
}:
delib.module {
  name = "zsh.history";

  myconfig.always = {
    zsh.plugins = [
      {
        name = "zsh-history-substring-search";
        src = pkgs.zsh-history-substring-search;
      }
      {
        name = "history-filter";
        src = pkgs.customZshPlugins.zsh-history-filter;
      }
    ];
  };

  home.always = {
    programs.zsh = {
      history = {
        expireDuplicatesFirst = true;
        save = 5000;
        share = true;
        size = 5000;
      };

      initContent = ''
        # https://github.com/MichaelAquilina/zsh-history-filter
        export HISTORY_FILTER_EXCLUDE=("TOKEN")
      '';
    };
  };
}
