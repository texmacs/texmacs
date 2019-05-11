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

from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from .protocol import texmacs_out

class Asymptote(Graph):
    def __init__(self, name):
        super(Asymptote, self).__init__()
        self.name = name
        try:
            p = Popen([self.name, "-version"], stderr=PIPE)
            ret, err = p.communicate()
            # WARN: The Version Info is in stderr
            if (p.returncode == 0):
                self.message = str(err)
        except OSError:
            pass

    def evaluate(self, code):
        png = self.get_png_path()
        path = png.split("?")[0]
        p = Popen([self.name, "-fpng", "-o", path], stdin=PIPE, stderr=PIPE)
        out, err = p.communicate(input=code)
        if (p.returncode == 0):
          texmacs_out ("file:" + png)
        else:
          texmacs_out ("verbatim:" + err)

