{
  delib,
  lib,
  pkgs,
  ...
}:
let
  mkDenoHook =
    name: text:
    pkgs.writeTextFile {
      inherit name;
      destination = "/bin/${name}";
      executable = true;
      text = ''
        #!${pkgs.coreutils}/bin/env -S ${lib.getExe pkgs.deno} run --allow-env=XDG_RUNTIME_DIR --allow-write
        ${text}
      '';
      checkPhase = ''
        export DENO_DIR="$TMPDIR/deno-cache"
        ${lib.getExe pkgs.deno} check "$target"
      '';
    };

  sessionStart = mkDenoHook "codex-session-start" ''
    type SessionPayload = {
      session_id?: unknown
    }

    const payload: SessionPayload = await new Response(Deno.stdin.readable).json()
    const sessionId = payload.session_id

    if (typeof sessionId !== "string" || sessionId.length === 0) {
      throw new TypeError("Session ID must be a non-empty string")
    }
    if (sessionId.includes("/")) {
      throw new TypeError("Session ID must not contain a slash")
    }

    const runtimeDir = Deno.env.get("XDG_RUNTIME_DIR")
    if (!runtimeDir) {
      throw new Error("XDG_RUNTIME_DIR is not set")
    }

    const sessionsDir = `''${runtimeDir}/codex/sessions`
    await Deno.mkdir(sessionsDir, { recursive: true })
    await Deno.writeTextFile(`''${sessionsDir}/''${sessionId}.pid`, `''${Deno.ppid}\n`)
  '';

  sessionEnd = mkDenoHook "codex-session-end" ''
    type SessionPayload = {
      session_id?: unknown
    }

    const payload: SessionPayload = await new Response(Deno.stdin.readable).json()
    const sessionId = payload.session_id

    if (typeof sessionId !== "string" || sessionId.length === 0) {
      throw new TypeError("Session ID must be a non-empty string")
    }
    if (sessionId.includes("/")) {
      throw new TypeError("Session ID must not contain a slash")
    }

    const runtimeDir = Deno.env.get("XDG_RUNTIME_DIR")
    if (!runtimeDir) {
      throw new Error("XDG_RUNTIME_DIR is not set")
    }

    try {
      await Deno.remove(`''${runtimeDir}/codex/sessions/''${sessionId}.pid`)
    } catch (error) {
      if (!(error instanceof Deno.errors.NotFound)) {
        throw error
      }
    }
  '';
in
delib.module {
  name = "agent-hooks.session-ids";

  options =
    with delib;
    moduleOptions {
      enable = boolOption true;
    };

  myconfig.ifEnabled = {
    codex.hooks = {
      SessionStart = [
        {
          matcher = "startup|resume";
          hooks = [
            {
              type = "command";
              command = lib.getExe sessionStart;
            }
          ];
        }
      ];
      SessionEnd = [
        {
          hooks = [
            {
              type = "command";
              command = lib.getExe sessionEnd;
            }
          ];
        }
      ];
    };
  };
}
