#!/usr/bin/env python
###############################################################################
##
## MODULE      : tm_graph.py
## DESCRIPTION : Launcher for the Graph plugin
## COPYRIGHT   : (C) 2004       Ero Carrera, ero@dkbza.org
##               (C) 2012       Adrian Soto
##               (C) 2014       Miguel de Benito Delgado, mdbenito@texmacs.org
##               (C) 2018-2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import sys
sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")

import re
import string
import time
import csv   # Used to parse scheme forms
from inspect   import ismodule, getsource, getsourcefile
from types     import CodeType
from io        import open
from io        import StringIO
from tmpy.protocol        import *
from tmpy.capture         import CaptureStdout
from tmpy.graph.asymptote import Asymptote
from tmpy.graph.dratex    import DraTeX
from tmpy.graph.graphviz  import Graphviz
from tmpy.graph.mermaid   import Mermaid
from tmpy.graph.plantuml  import PlantUML
from tmpy.graph.xypic     import XYpic
from tmpy.graph.tikz      import TikZ
from tmpy.graph.feynmf    import FeynMF
from tmpy.graph.latex     import LaTeX
from tmpy.graph.pdflatex  import PDFLaTeX
from tmpy.graph.gnuplot   import Gnuplot
from tmpy.compat          import *

#import logging as log
#log.basicConfig(filename='/tmp/tm_python.log',level=log.DEBUG)

init_time = time.time()

__version__  = '1.0'
__author__   = 'Darcy Shen'

my_globals   = {}

def compose_output(data):
    """Do some parsing on the output according to its type.
    
    Non printable characters in unicode strings are escaped
    and objects of type None are not printed (so that procedure calls,
    as opposed to function calls, don't produce any output)."""

    if py_ver == 3: cl = str
    else:           cl = unicode
    if isinstance(data, cl):
        data2 = r''
        for c in data:
            if c not in string.printable:
                data2 += '\\x%x' % ord(c)
            else:
                data2 += c
        data = data2
    if data is None:
        data = ''
    return 'verbatim:%s' % str(data).strip()

def as_scm_string (text):
    return '"%s"' % text.replace('\\', '\\\\').replace('"', '\\"')

###############################################################################
## Session start
###############################################################################

if py_ver == 3:
    text = 'import builtins as __builtins__'
else:
    text = 'import __builtin__ as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_graph")

# Reopen stdout unbufferd (flush after each stdout.write() and print)
if py_ver == 3:
    sys.stdout = os.fdopen (sys.stdout.fileno(), 'w')
else:
    sys.stdout = os.fdopen (sys.stdout.fileno(), 'w', 0)

grapvizs = list(map(lambda x: Graphviz(x), ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork", "osage"]))
others = [Asymptote(), PlantUML(), Mermaid(), XYpic(), TikZ(),
          FeynMF(), LaTeX(), PDFLaTeX(), Gnuplot(), DraTeX()]
graphs = list(filter(lambda x: x.available(), grapvizs + others))
graph_names = list(map(lambda x: x.name, graphs))

if len(graphs) == 0:
    flush_verbatim ("\nSorry, please check your installation of Graphviz/Asymptote")
    flush_prompt ("dead] ")
    exit(0)

current = graphs[0]
warmup_time_ms = int((time.time() - init_time) * 1000)


flush_verbatim ("Generate graphs with your favorite tools in GNU TeXmacs\n")
flush_verbatim ("Created by Darcy Shen, Implemented in Python, " + current.name + "[" + str(warmup_time_ms) + "ms] by default\n")
flush_verbatim ("Welcome to star and fork it at https://github.com/texmacs/plugins\n")
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
