#!/usr/bin/env python
###############################################################################
##
## MODULE      : tm_xypic.py
## DESCRIPTION : Launcher for the Xy-pic plugin
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import sys
sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")

from tmpy.protocol        import *
from tmpy.graph.xypic     import XYpic
from tmpy.capture         import CaptureStdout
from tmpy.compat          import *

my_globals   = {}

if py_ver == 3:
    text = 'import builtins as __builtins__'
else:
    text = 'import __builtin__ as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_xypic")

current = XYpic()
current.greet()
current.main_loop()
