#!/usr/bin/env python
###############################################################################
##
## MODULE      : diagrams.py
## DESCRIPTION : Diagrams plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph     import Graph
from ..protocol import *
from ..compat   import *

class Diagrams(Graph):
    def __init__(self, name):
        super(Diagrams, self).__init__()
        self.name = name
        self.width = 600

    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen(["diagrams", "--version"], stdout=PIPE)
                ret, err = p.communicate()
                if (p.returncode == 0):
                    self.message = ret.decode()
            except OSError:
                pass
        super(Diagrams, self).greet()

    def available(self):
        return which("diagrams") is not None

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".txt"
        with open(code_path, 'w') as code_file:
            code_file.write(code)

        cmd = ["diagrams", self.name, code_path, self.get_svg_path()]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
          flush_file (self.get_svg())
        else:
          flush_verbatim (err.decode())
