{ delib, host, inputs, ... }:
delib.module {
  name = "agent-skills.personal.public";

  options = with delib; moduleOptions {
    enable = boolOption host.codingFeatured;
  };

  myconfig.ifEnabled = {
    agent-skills.sources.emacs-config = {
      path = inputs.emacs-config.outPath + "/skills";
      priority = 3;
    };
  };
}
