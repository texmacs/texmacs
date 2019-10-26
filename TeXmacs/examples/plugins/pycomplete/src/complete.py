#!/usr/bin/env python
###############################################################################
##
## MODULE      : complete.py
## DESCRIPTION : Complete implemented in Python
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

from tmpy.protocol        import *
from tmpy.compat          import *

flush_verbatim ("We know how to complete 'h'")

while True:
    line = tm_input()
    if not line:
        pass
    if line[0] == DATA_COMMAND:
        flush_scheme ("""(tuple "h" "ello" "i there" "ola" "opsakee")""")
    else:
        flush_verbatim ("You typed " + line)
