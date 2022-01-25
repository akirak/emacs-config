{ pkgs, ... }:
{
  services = {
    postgresql = {
      enable = false;

      # The default is 5432, but I will change it to a different value
      # in case I make a mistake
      port = 5431;

      # Specify an explicit major version
      package = pkgs.postgresql_14;

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
        "postgres"
      ];

      # Allow access from web applications
      enableTCPIP = true;

      # Set the data directory explicitly
      dataDir = "/var/lib/postgresql/14";
    };

    postgresqlBackup = {
      enable = true;
      startAt = "*-*-* 01:15:00";
      # Because this is a development machine, backup is actually unnecessary,
      # so only backup the main database. Backup is just a practice here.
      databases = [
        "postgres"
      ];
    };
  };
}
