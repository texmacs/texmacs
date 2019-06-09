#!/usr/bin/env python
###############################################################################
##
## MODULE      : latex.py
## DESCRIPTION : LaTeX graphics support
## COPYRIGHT   : (C) 2019  Darcy Shen and Massimiliano Gubinelli
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from .protocol import *


class LaTeX(Graph):
    def __init__(self, name = "latex"):
        super(LaTeX, self).__init__()
        self.name = name
        self.message = "TeXmacs interface to a LaTeX picture generator"

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".tex"
        dvi_path = self.get_tmp_dir() + self.name + ".dvi"

        with open(code_path, 'w') as code_file:
            code_file.write(code)

        cmd0 = ["latex", "--interaction=errorstopmode", "-halt-on-error", code_path]
        cmd1 = ["dvips", "-q", dvi_path, "-o", self.get_eps_path()]
        ## for some reasons tikz does not work with the -E option to dvips...
        ## anyway the bounding box seems to be taken into account corretly even without.

        os.chdir(self.get_tmp_dir())
        p = Popen(cmd0, stdout=PIPE, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
            os.chdir(self.get_tmp_dir())
            p = Popen(cmd1, stdout=os.open(os.devnull, os.O_RDWR), stderr=PIPE)
            out, err = p.communicate()
            if (p.returncode == 0):
                flush_file (self.get_eps())
            else:
                flush_verbatim ("dvips error!\n" + err)
        else: 
            flush_verbatim ("LaTeX error!\n" + out)
