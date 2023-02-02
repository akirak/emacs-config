{pkgs, ...}: {
  services = {
    postgresql = {
      enable = true;

      # Default: 5432
      port = 5432;

      # Specify an explicit major version
      # package = pkgs.postgresql_14;

      # Trust local access
      authentication = pkgs.lib.mkOverride 12 ''
        local all all trust
        host all all localhost trust
      '';

      ensureUsers = [
        {
          name = "postgres";
        }
      ];
      ensureDatabases = [
        "metasub_dev"
      ];

      # Allow access from web applications
      enableTCPIP = true;

      # Set the data directory explicitly
      # dataDir = "/var/lib/postgresql/14";
    };

    pgadmin = {
      # Currently fails to build
      # enable = true;
      # Default
      port = 5050;
      initialEmail = "akira.komamura@gmail.com";
      initialPasswordFile = "/persist/etc/pgpasswd";
    };

    postgresqlBackup = {
      enable = true;
      startAt = "*-*-* 01:15:00";
      location = "/var/backup/postgresql";
      # Use the transparent compression of ZFS instead
      compression = "none";
      backupAll = true;
    };
  };
}
