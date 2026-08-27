{
  delib,
  lib,
  pkgs,
  host,
  ...
}:
let
  script = pkgs.writeText "stop-notification.ts" ''
    type StopPayload = {
      cwd: string
      hook_event_name: "Stop"
      last_assistant_message: string | null
    }

    const payload: StopPayload = await new Response(Deno.stdin.readable).json()
    const message = [
      `From an agent running in: ''${payload.cwd}`,
      "",
      payload.last_assistant_message ?? "(no assistant message)",
    ].join("\n")

    const command = new Deno.Command(${builtins.toJSON (lib.getExe pkgs.notify-desktop)}, {
      args: ["Stopped the codex session", message],
    })
    const status = await command.spawn().status
    Deno.exit(status.code)
  '';

  stopNotification = pkgs.writeShellApplication {
    name = "stop-notification";
    text = ''
      exec ${lib.getExe pkgs.deno} run \
        --allow-run=${lib.getExe pkgs.notify-desktop} \
        ${script}
    '';
  };
in
delib.module {
  name = "agent-hooks.stop-notification";

  options =
    with delib;
    moduleOptions {
      enable = boolOption host.guiFeatured;
    };

  myconfig.ifEnabled = {
    codex.hooks.Stop = [
      {
        hooks = [
          {
            type = "command";
            async = true;
            timeout = 10;
            command = lib.getExe stopNotification;
          }
        ];
      }
    ];
  };
}
