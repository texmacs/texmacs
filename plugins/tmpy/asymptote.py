#!/usr/bin/env python
###############################################################################
##
## MODULE      : asymptote.py
## DESCRIPTION : Asymptote plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from .protocol import *
from .compat import which

class Asymptote(Graph):
    def __init__(self, name = "asy"):
        super(Asymptote, self).__init__()
        self.name = name

    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen([self.name, "-version"], stderr=PIPE)
                ret, err = p.communicate()
                # WARN: The Version Info is in stderr
                if (p.returncode == 0):
                    self.message = err.decode()
            except OSError:
                pass
        super(Asymptote, self).greet()

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".asy"
        with open(code_path, 'w') as code_file:
            code_file.write(code)

        cmd = [self.name, "-quiet", "-feps", "-o", self.get_eps_path(), code_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
          flush_file (self.get_eps())
        else:
          flush_verbatim (err.decode())
