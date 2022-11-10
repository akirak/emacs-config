final: prev: {
  github-linguist = prev.callPackage ./development/github-linguist {};

  shippori-mincho = prev.callPackage ./fonts/shippori-mincho.nix {};
  jetbrains-mono-nerdfont = prev.callPackage ./fonts/jetbrains-mono-nerdfont.nix {};

  bubblewrapGUI = prev.callPackage ./security/bubblewrap-gui.nix {};

  emacsSandboxed = prev.callPackage ./development/emacs-sandboxed {} {
    inherit (final) bubblewrapGUI;
  };

  listEmacsProjects = final.writeShellScriptBin "ls-emacs-projects" ''
    set -euo pipefail

    while [[ $# -gt 0 ]]
    do
      case "$1" in
        # This option can be helpful in trivial shell functions where you don't
        # want to set separate `set -euo pipefail` option.
        --pipe)
          pipe="$2"
          shift
          ;;
      esac
      shift
    done

    # /tmp is protected, so use another directory
    tmp=$(mktemp -p "''${XDG_RUNTIME_DIR}")

    trap "rm -f '$tmp'" ERR EXIT

    # If the server isn't running, this script will exit with 1.
    ${final.emacsclient}/bin/emacsclient --eval "(with-temp-buffer
        (insert (mapconcat #'expand-file-name (project-known-project-roots) \"\n\"))
        (write-region (point-min) (point-max) \"$tmp\"))" > /dev/null

    if [[ -v pipe ]]
    then
      cat "$tmp" | $pipe
    else
      cat "$tmp"
    fi
  '';

  readability-cli = prev.callPackage ./media/readability-cli {
    pkgs = prev;
  };

  nodePackages =
    prev.nodePackages
    // {
      mermaid-cli = prev.nodePackages.mermaid-cli.overrideAttrs (old: {
        passthru =
          (old.passthru or {})
          // {
            exePath = "/bin/mmdc";
          };
      });
    };
}
