-- | Actions for XMonad.

module Actions where

import XMonad
import qualified XMonad.Actions.PhysicalScreens as P
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.List as L
import XMonad.Util.Run (runInTerm)

-- | Focus a window or run a command via rofi.
rofi :: X ()
rofi = spawn "rofi -show combi -combi-modi window,drun,run"

-- | If there is an open window, focus on it. If there is none, quit XMonad.
confirmQuit :: X ()
confirmQuit = do
  ws <- withWindowSet $ return . W.allWindows
  if null ws
    then io (exitWith ExitSuccess)
    else windows $ W.focusWindow (head ws)

-- | Switch to a workspace whose name starts with the given string.
viewWorkspaceByPrefix :: String -> X ()
viewWorkspaceByPrefix prefix = windows $ \s ->
  case findTagByPrefix prefix s of
    Just i -> W.view i s
    _ -> s

-- | Move the window to a workspace whose name starts with the given string.
shiftToWorkspaceByPrefix :: String -> X ()
shiftToWorkspaceByPrefix prefix = windows $ \s ->
  case findTagByPrefix prefix s of
    Just i -> W.shift i s
    _ -> s

-- | Find the tag of a workspace that has a given prefix.
findTagByPrefix prefix s =
  L.find (prefix `L.isPrefixOf`) $ map W.tag $ W.workspaces s

viewWorkspaceWithAction i =
  do viewWorkspaceByPrefix (show i)
     withWindowSet $ \s ->
                       case W.peek s of
                         Nothing -> runWorkspaceAction
                         _ -> return ()

viewScreen = P.viewScreen P.horizontalScreenOrderer

sendToScreen = P.sendToScreen P.horizontalScreenOrderer

-- | Swap two visible workspaces.
swapScreens :: X ()
swapScreens =
  windows $ \s ->
    case W.visible s of
      [] -> s
      (screen:_) ->
        W.greedyView (W.tag (W.workspace screen)) s

-- | Run an action depending on the current tag.
runWorkspaceAction :: X ()
runWorkspaceAction = do
  ws <- gets windowset
  let tag = W.currentTag ws
  case L.break (== '-') tag of
    (_,[]) -> return ()
    (_,(_:suffix)) ->  go suffix
    where
      go :: String -> X ()
      go "emacs" = spawn "emacs-unsafe"
      go "web" = spawn "firefox --new-window"
      -- go "github" = firefox "https://github.com"
      -- go "music" = firefox "https://music.youtube.com"
      go _ = return ()

-- | Open a URL with the browser.
chromium :: String -> X ()
chromium url = spawn $ "chromium --new-window " ++ url

firefox :: String -> X ()
firefox url = spawn $ "firefox --new-window " ++ url

switchNixOSConfig :: X ()
switchNixOSConfig = runInTerm ""
    "sh -c 'sudo nixos-rebuild switch --flake `readlink -f $HOME/config`#`uname -n` || read'"
