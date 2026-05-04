# The user module.
# Based on https://github.com/yunfachi/nix-config/tree/9ba35d6fc96a4eb86db72c91a0fc74e636c71f82/modules/toplevel/user.nix
{
  delib,
  ...
}:
delib.module {
  name = "user";

  options.user = with delib; {
    # Should be added to users.users.${username}.extraGroups on NixOS
    extraGroups = listOfOption str [ ];
  };
}
