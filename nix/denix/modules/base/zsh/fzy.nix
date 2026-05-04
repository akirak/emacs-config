{ delib, pkgs, ... }:
delib.module {
  name = "zsh";

  myconfig.always = {
    zsh.plugins = [
      {
        name = "fzy";
        src = pkgs.customZshPlugins.zsh-fzy;
      }
    ];
  };

  home.ifEnabled = {
    programs.zsh = {
      initContent = ''
        # Configuration for zsh-fzy plugin https://github.com/aperezdc/zsh-fzy
        bindkey '\eq' fzy-proc-widget
        bindkey '\ew' fzy-cd-widget
        bindkey '\ee' fzy-file-widget
        bindkey '\er' fzy-history-widget
        zstyle :fzy:file command fd -t f
        zstyle :fzy:cd command fd -t d

        function pick() {
          local arg
          fzy | read -r arg && "$@" "$arg"
        }

        function mountpoints() {
          findmnt -oTARGET --list --noheadings
        }

        function remotes() {
          git rev-parse --show-toplevel >/dev/null || return 1
          git --no-pager config --local --list \
            | sed -n -E 's/^remote\..+?\.url=(.+)/\1/p' \
            | xargs realpath -q -s -e
        }

        function projects() {
          {
            tmp=$(mktemp -p "''${XDG_RUNTIME_DIR}")
            trap "rm -f '$tmp'" ERR EXIT
            # If the server isn't running, this script will exit with 1.
            emacsclient --eval "(with-temp-buffer
               (insert (string-join
                        (thread-last
                          (project-known-project-roots)
                          (append (thread-last
                                    (frame-list)
                                    (mapcan #'window-list)
                                    (mapcar #'window-buffer)
                                    (mapcar (lambda (buffer)
                                              (buffer-local-value 'default-directory buffer)))))
                          (mapcar #'expand-file-name)
                          (seq-uniq))
                        \"\\n\"))
               (write-region (point-min) (point-max) \"$tmp\"))" > /dev/null
            cat "$tmp"
          }
        }

        function project-subdirectories() {
          local root="$(git rev-parse --show-toplevel)"
          if [[ -z "$root" ]]; then
            return 1
          else
            ( cd "$root" && fd -a -L -t d )
          fi
        }

        function emacs-visible-directories() {
            tmp=$(mktemp -p "''${XDG_RUNTIME_DIR}")
            trap "rm -f '$tmp'" ERR EXIT
            # If the server isn't running, this script will exit with 1.
            emacsclient --eval "(with-temp-buffer
               (insert (string-join
                          (thread-last
                            (mapcar #'window-list (get-mru-frames 'visible))
                            (flatten-list)
                            (mapcar #'window-buffer)
                            (mapcar (lambda (buffer)
                                      (expand-file-name
                                       (buffer-local-value 'default-directory buffer))))
                            (seq-uniq))
                        \"\\n\"))
               (write-region (point-min) (point-max) \"$tmp\"))" > /dev/null
            cat "$tmp"
        }

        function cdv() {
          builtin cd "$1" && pwd
        }

        function cd() {
          case "$1" in
            -h)
              echo <<-HELP
                Usage: cd [-p|-m|-r|DIR]

                Options:
                  -p: Select an Emacs project (requires an Emacs server running)
                  -m: Select a mount point
                  -r: Select a remote of the current Git repository
                  -w: Select the directory of a visible buffer
                  -d: Select a sub-directory inside the current Git repository
                  -g: Go to the root of the current Git working tree
        HELP
              ;;
            -p|)
              projects | pick cdv
              ;;
            -m)
              mountpoints | pick cdv
              ;;
            -r)
              remotes | pick cdv
              ;;
            -w)
              emacs-visible-directories | pick cdv
              ;;
            -d)
              project-subdirectories | pick cdv
              ;;
            -g)
              local root="$(git rev-parse --show-toplevel)"
              if [[ -n "$root" ]]; then
                builtin cd "$root"
              fi
              ;;
            *)
              builtin cd "$@"
              ;;
          esac
        }
      '';
    };

    home.packages = with pkgs; [
      fzy
    ];
  };
}
