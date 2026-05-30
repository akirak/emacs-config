{ delib, pkgs, ... }:
delib.module {
  name = "zsh";

  home.ifEnabled = {
    programs.zsh.initContent = ''
      function focus-emacs() {
        emacsclient -n -u -e "(select-frame-set-input-focus (selected-frame))"
      }

      function dired() {
        local file
        local in_terminal=0

        case "$1"; in
          -n)
            in_terminal=1
            shift
            ;;
          *)
            echo >&2 "Opening in an existing Emacs frame, if any."
            ;;
        esac

        if [[ $# -gt 0 ]]; then
          file="$1"
        else
          file="."
        fi

        if [[ $in_terminal -eq 1 ]]; then
          emacsclient -nw -u -a emacs "$file"
        else
          if [[ -f "$file" ]]; then
            emacsclient -n -u -a emacs -e "(dired-jump nil \"$file\")" \
              && focus-emacs
          else
            emacsclient -n -u -a emacs -e "(dired \"$file\")" \
              && focus-emacs
          fi
        fi
      }
    '';
  };
}
