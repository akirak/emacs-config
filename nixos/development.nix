{
  imports = [
    # ./profiles/postgresql.nix
    # ./profiles/podman.nix
  ];

  services.nginx = {
    enable = true;
    virtualHosts.default = {
      rejectSSL = true;
      locations."/".proxyPass = "http://localhost:3000";
      serverAliases = [
        "localhost"
      ];
    };
  };
}
