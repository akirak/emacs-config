{ pkgs, ... }:
{
  home.packages = [
    pkgs.rclone
  ];

  # Based on an example code on https://discourse.nixos.org/t/fusermount-systemd-service-in-home-manager/5157
  systemd.user.services."dropbox-mountpoint" =
    {
      Unit = {
        Description = "Mount Dropbox as a file system using rclone";
      };
      # This assumes the remote is already configured as 'dropbox'.
      # Maybe I'll add a secret to this repository later.
      Service = {
        Type = "notify";
        Restart = "always";
        RestartSec = "20s";
        # TODO: Maybe add vfs options.
        ExecStart = "${pkgs.rclone}/bin/rclone mount dropbox: %h/storages/dropbox";
        # Ensure the directory for the mountpoint.
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p %h/storages/dropbox";
        ExecStop = "/run/wrappers/bin/fusermount -u $h/storage/dropbox";
        Environment = [ "PATH=/run/wrappers/bin:$PATH" ];
      };
      # No Install section
    };
}
