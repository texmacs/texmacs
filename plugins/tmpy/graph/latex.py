#!/usr/bin/env python
###############################################################################
##
## MODULE      : latex.py
## DESCRIPTION : LaTeX graphics support
## COPYRIGHT   : (C) 2019-2020  Darcy Shen and Massimiliano Gubinelli
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from ..compat import which
from ..protocol import *


class LaTeX(Graph):
    def __init__(self, name = "latex"):
        super(LaTeX, self).__init__()
        self.name = name
        self.message = "TeXmacs interface to a LaTeX picture generator"

    def available(self):
        return all(list(map(lambda x: which(x) is not None, ["latex", "dvips"])))

    def kpsewhich(self, name):
        cmd = ["kpsewhich", name]
        p = Popen(cmd, stdout=PIPE, stderr=STDOUT)
        out, err = p.communicate()
        if (p.returncode == 0):
            return out
        else:
            return ""

    def evaluate(self, code):

        if not (code.lstrip().startswith("\\documentclass")):
            flush_err("I expect a valid LaTeX document, usually starting with\n"
                +"   \\documentclass{standalone}\n"
                +"which will take proper care of suitable bounding box for the image.\n"
                +"The document is sent to latex and then to dvips to produce a single image.")    
            flush_verbatim("")
            return

        code_path = self.get_tmp_dir() + self.name + ".tex"
        dvi_path = self.get_tmp_dir() + self.name + ".dvi"

        with open(code_path, 'w') as code_file:
            code_file.write(code)

        cmd0 = ["latex", "--interaction=errorstopmode", "-halt-on-error", code_path]
        cmd1 = ["dvips", "-q", dvi_path, "-o", self.get_eps_path()]

        # flush_err (code) # to debug
        phase = "LaTeX"    
        #flush_err ("Running " + phase)
        os.chdir(self.get_tmp_dir())
        p = Popen(cmd0, stdout=PIPE, stderr=STDOUT)
        out, err = p.communicate()
        if (p.returncode == 0):
            phase = "dvips"    
            #flush_err ("Running " + phase)
            p = Popen(cmd1, stdout=PIPE, stderr=STDOUT)
            out, err = p.communicate()
            if (p.returncode == 0):
                phase = "Finished"    
                #flush_err ("Running " + phase)
                flush_file (self.get_eps())
                return
        flush_err (phase + " error!\n" + out.decode())
        flush_verbatim("")   # otherwise TeXmacs keeps waiting output
            