{
  delib,
  inputs,
  pkgs,
  host,
  ...
}:
delib.module {
  name = "zen-browser";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;
    };

  myconfig.ifEnabled = {
    browser.enable = true;
    # Set browser.desktopFile if available.
  };

  home.always.imports = [
    inputs.zen-browser.homeModules.default
  ];

  home.ifEnabled =
    { ... }:
    {
      programs.zen-browser = {
        enable = true;

        # Use the stable version instead of the default beta.
        package = pkgs.zen-browser;

        # Common policies from
        # https://github.com/0xc000022070/zen-browser-flake?tab=readme-ov-file#some-common-policies.
        # See https://mozilla.github.io/policy-templates/ for options.
        policies = {
          AutofillAddressEnabled = true;
          AutofillCreditCardEnabled = false;
          DisableAppUpdate = true;
          DisableFeedbackCommands = true;
          DisableFirefoxStudies = true;
          DisablePocket = true;
          DisableTelemetry = true;
          DontCheckDefaultBrowser = true;
          NoDefaultBookmarks = true;
          OfferToSaveLogins = false;
          EnableTrackingProtection = {
            Value = true;
            Locked = true;
            Cryptomining = true;
            Fingerprinting = true;
          };
        };

        # TODO: Configure profiles

        # profiles.default = {
        # };
      };
    };
}
