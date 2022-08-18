{
  writeShellScriptBin,
  notify-desktop,
}: let
  notify = "${notify-desktop}/bin/notify-desktop -r nixos-rebuild";
in
  writeShellScriptBin "nixos-rebuild-and-notify" ''
    cd $HOME/config
    if nixos-rebuild switch --flake `readlink -f $HOME/config`#`uname -n` \
        --print-build-logs --use-remote-sudo; then
      ${notify} -t 5000 'nixos-rebuild successfully finished'
    else
      ${notify} -t 5000 'nixos-rebuild has failed'
      read
    fi
  ''
