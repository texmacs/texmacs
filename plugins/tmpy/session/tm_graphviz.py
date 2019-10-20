#!/usr/bin/env python
###############################################################################
##
## MODULE      : tm_graphviz.py
## DESCRIPTION : Launcher for the Graphviz plugin
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

from tmpy.graph.graphviz  import Graphviz
from tmpy.capture         import CaptureStdout
from tmpy.compat          import py_ver
from tmpy.protocol        import *
from tmpy.compat          import *

my_globals   = {}

if py_ver == 3:
    text = 'import builtins as __builtins__'
else:
    text = 'import __builtin__ as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_graphviz")

graphs = list(map(lambda x: Graphviz(x), ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"]))
graph_names = list(map(lambda x: x.name, graphs))

if len(graphs) == 0:
    flush_verbatim ("\nSorry, please check your installation of Graphviz")
    flush_prompt ("dead] ")
    exit(0)

current = graphs[0]
current.greet()
flush_prompt (current.name + "] ")

def unigraph(text):
    magic_lines = text.split("\n")
    magic_line = magic_lines[0]
    code = '\n'.join(magic_lines[1:])
    command = magic_line.split(" ")[0].strip("%")

    if command in graph_names:
        graph = graphs[graph_names.index(command)]
        graph.apply_magic(magic_line)
        graph.eval(code)
    else:
        flush_verbatim ("No such Graphs backend: " + command)


# Main session loop.
while True:
    line = tm_input()
    if not line:
        continue
    if line[0] == DATA_COMMAND:
        # TODO: Handle completions
        continue
    else:
        lines = [line]
        while line != "<EOF>":
            line = tm_input()
            if line == '': 
                continue
            lines.append(line)

        text='\n'.join(lines[:-1])

        if text.startswith("%"):
            unigraph(text)
        elif text == "help":
            flush_verbatim ("[help, " + ", ".join(str(x) for x in graph_names) + "]\n")
            current.greet()
        elif text in graph_names:
            current = graphs[graph_names.index(text)]
            current.greet()
        else:
            current.eval(text)
