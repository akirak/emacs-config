{
  lib,
  symlinkJoin,
  notify-desktop,
  writeShellScriptBin,
  makeDesktopItem,
}: {
  nixConfigDir,
  siteConfigDir,
}: let
  makeInstaller = name: {userEmacsDirectory, ...}:
    writeShellScriptBin "${name}-installer" ''
      cd ${nixConfigDir}
      echo "Building Emacs..."

      ${notify-desktop}/bin/notify-desktop \
        -t 2000 "Started building Emacs..."

      start=$(date +%s)

      if nix profile install .#${name} \
        --override-input site $(readlink -f "${siteConfigDir}") \
        --priority `date +%s` \
        --print-build-logs; then

          printf "Build finished in %d sec\n" $(($(date +%s) - start))

          echo Installed

          nix profile list \
          | grep -E '-${name}$' \
          | cut -d' ' -f1 \
          | head --lines=-1 \
          | xargs nix profile remove

          ${notify-desktop}/bin/notify-desktop \
            -t 5000 "Reinstalled ${name} to the profile"
      else
        ${notify-desktop}/bin/notify-desktop \
          -t 5000 "Failed to install ${name} to the profile"
      fi
    '';

  makeDesktopItemForProfile = name: _:
    makeDesktopItem {
      inherit name;
      desktopName = "GNU Emacs (${name})";
      icon = "emacs";
      exec = name;
      terminal = false;
    };
in
  profiles:
    symlinkJoin {
      name = "emacs-installer";
      paths =
        lib.mapAttrsToList makeInstaller profiles
        ++ lib.mapAttrsToList makeDesktopItemForProfile profiles;
    }
