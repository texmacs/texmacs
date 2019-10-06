#!/usr/bin/env python
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
sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")

from tmpy.graph.dratex    import DraTeX
from tmpy.capture         import CaptureStdout
from tmpy.compat          import py_ver

my_globals   = {}

if py_ver == 3:
    text = 'import builtins as __builtins__'
else:
    text = 'import __builtin__ as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_dratex")

current = DraTeX()
current.greet()
current.main_loop()
