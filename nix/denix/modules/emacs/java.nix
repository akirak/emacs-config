{
  delib,
  pkgs,
  homeconfig,
  ...
}:
delib.module {
  name = "emacs.java";

  options =
    with delib;
    moduleOptions {
      enable = boolOption false;
      pluginPath = readOnly (
        strOption (homeconfig.programs.emacs-twist.directory + "/java-debug-plugin.jar")
      );
    };

  home.ifEnabled =
    { cfg, ... }:
    {
      home.file.${cfg.pluginPath}.source =
        pkgs.runCommand "java-debug-plugin.jar"
          {
            propagatedBuildInputs = [
              pkgs.vscode-extensions.vscjava.vscode-java-debug
            ];
          }
          ''
            jar=$(find ${pkgs.vscode-extensions.vscjava.vscode-java-debug}/share/vscode/extensions/vscjava.vscode-java-debug/server -name "*.jar")

            ln -s "$jar" $out
          '';
    };
}
