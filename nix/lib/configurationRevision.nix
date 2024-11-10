let
  inherit (builtins) substring;
in
self:
"${substring 0 8 self.lastModifiedDate}.${if self ? rev then substring 0 7 self.rev else "dirty"}"
