#!/usr/bin/env python
###############################################################################
##
## MODULE      : graphviz.py
## DESCRIPTION : Graphviz plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import sys
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from ..protocol import *

class Graphviz(Graph):
    def __init__(self, name):
        super(Graphviz, self).__init__()
        self.name = name
        self.default_output = "eps"
    
    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen([self.name, "-V"], stderr=PIPE)
                ret, err = p.communicate()
                # WARN: The Version Info is in stderr
                if (p.returncode == 0):
                    self.message = err.decode()
            except OSError:
                pass
        super(Graphviz, self).greet()

    def evaluate(self, code):
        # NOTE: the eps output format does not always work
        # for example, when we use Chinese in a label
        # by default, we use eps, however, we may switch to png via
        # the -output option on the magic line
        if self.output == "eps":
            path = self.get_eps_path()
            image = self.get_eps()
            cmd_list = [self.name, "-Teps"]
        elif self.output == "png":
            path = self.get_png_path()
            image = self.get_png()
            cmd_list = [self.name, "-Tpng"]
        elif self.output == "svg":
            path = self.get_svg_path()
            image = self.get_svg()
            cmd_list = [self.name, "-Tsvg"]
        else:
            flush_verbatim("Unsupported output type: " + self.output)
            return

        if os.path.isfile(path):
            os.remove(path)
        f = open(path, 'wb')
        p = Popen(cmd_list, stdout=f, stdin=PIPE, stderr=PIPE)
        py_ver = sys.version_info[0]
        if py_ver == 3:
            out, err = p.communicate(input=code.encode())
        else:
            out, err = p.communicate(input=code)
        if (p.returncode == 0):
            flush_file (image)
        else:
            flush_verbatim (err.decode())
