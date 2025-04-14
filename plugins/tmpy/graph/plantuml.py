#!/usr/bin/env python3
###############################################################################
##
## MODULE      : plantuml.py
## DESCRIPTION : PlantUML plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from ..protocol import *

class PlantUML(Graph):
    def __init__(self, name = "plantuml"):
        super(PlantUML, self).__init__()
        self.name = name

    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen([self.name, "-version"], stdout=PIPE)
                ret, err = p.communicate()
                if (p.returncode == 0):
                    self.message = ret.decode()
            except OSError:
                pass
        super(PlantUML, self).greet()

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".puml" 
        with open(code_path, 'w') as code_file:
            code_file.write(code)

        # Execute `plantuml -teps /path/to/plantuml.puml`
        cmd = [self.name, "-teps", code_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
            flush_file (self.get_eps())
        else:
            flush_verbatim (err.decode())
        
