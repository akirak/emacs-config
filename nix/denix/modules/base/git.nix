{
  delib,
  lib,
  pkgs,
  ...
}:

delib.module {
  name = "git";

  options =
    with delib;
    moduleOptions {
      enable = boolOption true;

      identities =
        attrsOfOption
          (submodule {
            options = {
              userName = noDefault (strOption null);
              userEmail = noDefault (strOption null);
              githubUser = strOption null;
              signingKey = strOption null;
              githubOrganizations = listOfOption str [ ];
              remoteUrls = listOfOption str [ ];
              gitdirs = listOfOption str [ ];
            };
          })
          {
            "akira.komamura@gmail.com" = {
              userName = "Akira Komamura";
              userEmail = "akira.komamura@gmail.com";
              githubUser = "akirak";
              signingKey = "5B3390B01C01D3E";
              githubOrganizations = [
                "emacs-twist"
                "elpa-mirrors"
              ];
              gitdirs = [
                "~/work2/foss/"
                "~/work2/learning/"
                "~/work2/personal/"
                "/git-annex/"
              ];
            };
          };
    };

  home.ifEnabled =
    { cfg, ... }:
    let
      makeGitConfig =
        {
          userName,
          userEmail,
          githubUser,
          signingKey,
        }:
        pkgs.writeText "config" ''
          [user]
          name = "${userName}"
          email = "${userEmail}"
          ${lib.optionalString (signingKey != null) ''
            signingKey = "${signingKey}"
          ''}
          ${lib.optionalString (githubUser != null) ''
            [github]
            user = "${githubUser}"
          ''}
        '';
    in
    {
      programs.git = {
        enable = true;

        signing.format = "openpgp";

        settings = {
          pull.rebase = true;

          merge.conflictstyle = "diff3";

          "url \"git@github.com:\"".pushInsteadOf = "https://github.com/";

          core.autocrlf = "input";

          # Only on WSL
          # core.fileMode = false;

          # Increase the size of post buffers to prevent hung ups of git-push.
          # https://stackoverflow.com/questions/6842687/the-remote-end-hung-up-unexpectedly-while-git-cloning#6849424
          http.postBuffer = "524288000";
        };

        ignores = [
          ".direnv"
          "result"
          "result-*"
          "#*"
          ".git-bak*"
          "*.local"
          "*.local.*"
        ];

        includes = lib.pipe cfg.identities [
          (lib.mapAttrsToList (
            _:
            {
              userName,
              userEmail,
              githubUser,
              signingKey,
              githubOrganizations,
              remoteUrls,
              gitdirs,
            }:
            let
              configFile = makeGitConfig {
                inherit
                  githubUser
                  signingKey
                  userName
                  userEmail
                  ;
              };

              allRemoteUrls =
                remoteUrls
                ++ (lib.flatten (
                  builtins.map (account: [
                    "https://github.com/${account}/**"
                    "git@github.com:${account}/**"
                  ]) ([ githubUser ] ++ githubOrganizations)
                ));

              conditions = lib.flatten (
                (builtins.map (remoteUrl: "hasconfig:remote.*.url:${remoteUrl}") allRemoteUrls)
                ++ (builtins.map (dir: "gitdir:${dir}") gitdirs)
              );
            in
            builtins.map (condition: {
              path = configFile;
              inherit condition;
            }) conditions
          ))
          lib.flatten
        ];
      };
    };
}
