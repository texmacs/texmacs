#!/usr/bin/env python3
###############################################################################
##
## MODULE      : tm_eukleides.py
## DESCRIPTION : Launcher for the eukleides plugin
## COPYRIGHT   : (C) 2021  Darcy Shen
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

from subprocess import Popen, PIPE, STDOUT
from tmpy.graph.graph import Graph
from tmpy.protocol import *
from tmpy.capture import CaptureStdout
from tmpy.compat import *


class Eukleides(Graph):
    def __init__(self, name = "eukleides"):
        super(Eukleides, self).__init__()
        self.name = name
        self.width = 600
        self.default_output = "eps"

    def greet(self):
        flush_latex ("E \\Upsilon K \\Lambda \\tmop{EI} \\Delta H \\Sigma")
        flush_newline ()
        flush_verbatim ("A Euclidean Geometry Drawing Language")
        flush_newline ()
        try:
            p = Popen([self.name, "-v"], stdout=PIPE)
            ret, err = p.communicate()
            # WARN: The Version Info is in stderr
            if (p.returncode == 0):
                self.message = ret.decode()
        except OSError:
            pass
        super(Eukleides, self).greet()

    def evaluate(self, code):
        if self.output == "eps":
            image = self.get_eps()
            image_path = self.get_eps_path()
        elif self.output == "png":
            image = self.get_png()
        elif self.output == "svg":
            image = self.get_svg()
        else:
            flush_verbatim("Unsupported output type: " + self.output)
            return

        code_path = self.get_tmp_dir() + self.name + ".txt"
        with open(code_path, 'w') as code_file:
            code_file.write(self.pre_code)
            code_file.write(code)

        cmd = [self.name, f"--output={image_path}", code_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
          flush_file (image)
        else:
          flush_verbatim (err.decode())

    def main_loop(self):
        # Main session loop.
        while True:
            line = tm_input()
            if not line:
                continue
            if line[0] == DATA_COMMAND:
                # TODO: Handle completions
                continue
            else:
                lines = []
                for x in line.split('~'):
                    lines.append(x)
                while line != "<EOF>":
                    line = tm_input()
                    for x in line.split('~'):
                        lines.append(x)
                text = '\n'.join(lines[:-1])
                self.eval(text)


if (exists (tmpy_home_path)):
        flush_verbatim ("WARNING: You are under develop mode using " + tmpy_home_path)
        flush_newline (2)

my_globals   = {}

text = 'import builtins as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_eukleides")

current = Eukleides()
if not current.available():
    flush_err ("Failed to launch the Eukleides plugin, aborted!!!")
    exit(-1)

current.greet()
current.main_loop()
