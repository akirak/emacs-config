{pkgs, ...}: {
  systemd.user.services."linkfarm@" = let
    script =
      pkgs.runCommand "linkfarm"
      {
        buildInputs = with pkgs; [
          bashInteractive
          coreutils
          findutils
        ];
        nativeBuildInputs = with pkgs; [
          makeWrapper
        ];
      } ''
        mkdir -p $out/bin
        cp ${./linkfarm.sh} $out/bin/linkfarm
        chmod u+x $out/bin/linkfarm
        patchShebangs $out/bin/linkfarm
        wrapProgram $out/bin/linkfarm \
          --prefix PATH : ${pkgs.coreutils}/bin \
          --prefix PATH : ${pkgs.findutils}/bin
      '';
  in {
    Unit = {
      Description = "Produce a linkfarm in the home directory";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${script}/bin/linkfarm '%I'";
    };

    Install = {
      WantedBy = ["default.target"];
    };
  };
}
