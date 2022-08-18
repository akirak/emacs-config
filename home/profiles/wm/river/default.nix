{pkgs, ...}: {
  imports = [
    ../../foot.nix
    ./dunst.nix
    ./waybar.nix
  ];

  home.packages = with pkgs; [
    # riverctl and rivertile need to be in PATH
    river

    wofi
    (pkgs.writeShellApplication {
      name = "lock-screen";
      runtimeInputs = [pkgs.swaylock-effects];
      # TODO: Use a color scheme
      text = ''
        swaylock -f --clock --fade-in 0.5
      '';
    })

    (pkgs.callPackage ./rebuild.nix {})
    flameshot
    wf-recorder
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

    Install = {
      Wants = ["dunst.service"];
    };
  };
}
