module Queries
  where

import XMonad.Core
import XMonad.ManageHook

qEmacs :: Query Bool
qEmacs = appName =? "emacs"

qFirefox :: Query Bool
qFirefox = className =? "firefox"
