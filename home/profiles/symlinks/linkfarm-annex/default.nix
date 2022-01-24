{ pkgs, ... }:
{
  systemd.user.services."linkfarm-annex" =
    let
      dir = pkgs.runCommand "linkfarm"
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
        cp ${./linkfarm-annex.bash} $out/bin/linkfarm-annex
        chmod u+x $out/bin/linkfarm-annex
        patchShebangs $out/bin/linkfarm-annex
        wrapProgram $out/bin/linkfarm-annex \
          --prefix PATH : ${pkgs.coreutils}/bin \
          --prefix PATH : ${pkgs.findutils}/bin
      '';
    in
    {
      Unit = {
        Description = "Produce symbolic links to annex repositories from the home directory";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${dir}/bin/linkfarm-annex";
      };

      Install = {
        WantedBy = [
          "default.target"
        ];
      };
    };
}
