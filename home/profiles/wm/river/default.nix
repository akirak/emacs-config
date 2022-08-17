{pkgs, ...}: {
  home.packages = with pkgs; [
    river
    wofi
  ];

  systemd.user.services.river = {
    Unit = {
      Description = "River Wayland compositor";
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.bashInteractive}/bin/bash --login -c 'river -c ${./init}'";

      Environment = [
        "XKB_DEFAULT_LAYOUT=us"
        "XKB_DEFAULT_OPTIONS=ctrl:nocaps"
      ];
    };
  };
}
