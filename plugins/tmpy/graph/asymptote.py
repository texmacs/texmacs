#!/usr/bin/env python3
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
from ..protocol import *
from ..compat import *

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

        cmd = [self.name, "-quiet", "-feps", "-o", self.get_eps_path_prefix(), code_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
          flush_file (self.get_eps())
        else:
          err_msg = err.decode()
          flush_verbatim (err_msg)
          # extra notification of missing semicolon
          if "unexpected end of input" in err_msg or "does not exist" in err_msg:
            flush_verbatim ("\nNote: This error might be related to your input, such as a missing semicolon.")

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
                text='\n'.join(lines[:-1])
                self.eval(text)

