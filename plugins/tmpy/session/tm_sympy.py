#!/usr/bin/env python
###############################################################################
##
## MODULE      : tm_sympy.py
## DESCRIPTION : Launcher for the SymPy plugin
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import sys
from os.path import exists
tmpy_home_path= os.environ.get("TEXMACS_HOME_PATH") + "/plugins/tmpy"
if (exists (tmpy_home_path)):
    sys.path.append(os.environ.get("TEXMACS_HOME_PATH") + "/plugins/")
else:
    sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")


from tmpy.protocol import *
from tmpy.compat import *
from tmpy.completion import parse_complete_command, complete
from tmpy.postscript import *
from tmpy.capture import CaptureStdout

from sympy.printing import latex
from sympy import Basic, MatrixBase
import sympy
import platform

my_globals   = {}

# We insert into the session's namespace the 'ps_out' method.
my_globals['ps_out'] = ps_out

# As well as some documentation.
my_globals['__doc__'] = """A SymPy plugin for TeXmacs."""

def flush_output (data):
    if isinstance (data, Basic) or isinstance (data, MatrixBase):
        flush_latex (latex (data))
    else:
        flush_verbatim (str(data).strip())


###############################################################################
# Session start
###############################################################################
if (exists (tmpy_home_path)):
    flush_verbatim ("WARNING: You are under develop mode using " + tmpy_home_path)
    flush_newline (2)
flush_verbatim ("SymPy %s under Python %s\n" % (sympy.__version__, platform.python_version()))
flush_verbatim ("Please see the documentation in Help -> Plugins -> SymPy\n")
flush_prompt (">>> ")

while True:
    line= tm_input ()
    if not line:
        continue
    if line[0] == DATA_COMMAND:
        sf = parse_complete_command (line[1:])
        if sf[0] == 'complete':
            flush_scheme (complete (sf[1], sf[2], my_globals))
        continue

    lines = [line]
    while line != "<EOF>":
        line = tm_input()
        if line == '': 
            continue
        lines.append(line)
    text = '\n'.join(lines[:-1])
    try: # Is it an expression?
        result = eval(text, my_globals)
    except:
        result = CaptureStdout.capture(text, my_globals, "tm_sympy")
    flush_output (result)
