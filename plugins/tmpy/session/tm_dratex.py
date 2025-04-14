#!/usr/bin/env python3
###############################################################################
##
## MODULE      : tm_dratex.py
## DESCRIPTION : Launcher for the DraTex plugin
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import sys
from os.path import exists
tmpy_home_path = os.environ.get("TEXMACS_HOME_PATH") + "/plugins/tmpy"
if (exists (tmpy_home_path)):
    sys.path.append(os.environ.get("TEXMACS_HOME_PATH") + "/plugins/")
else:
    sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")

from tmpy.graph.dratex    import DraTeX
from tmpy.capture         import CaptureStdout
from tmpy.compat          import py_ver
from tmpy.protocol        import *

if (exists (tmpy_home_path)):
    flush_verbatim ("WARNING: You are under develop mode using " + tmpy_home_path)
    flush_newline (2)

my_globals   = {}

if py_ver == 3:
    text = 'import builtins as __builtins__'
else:
    text = 'import __builtin__ as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_dratex")

current = DraTeX()
current.greet()
current.main_loop()
