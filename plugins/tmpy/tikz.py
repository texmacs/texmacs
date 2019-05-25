#!/usr/bin/env python
###############################################################################
##
## MODULE      : tikz.py
## DESCRIPTION : TikZ support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from .protocol import *


class TikZ(Graph):
    def __init__(self, name = "tikz"):
        super(TikZ, self).__init__()
        self.name = name

        self.pre_code = """
\\documentclass[tikz]{standalone}
\\begin{document}
\\begin{tikzpicture}
"""
        self.post_code = """
\\end{tikzpicture}
\\end{document}
"""
        self.message = "TeXmacs interface to TikZ"
        self.width = "640px"

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".tex"
        dvi_path = self.get_tmp_dir() + self.name + ".dvi"
        with open(code_path, 'w') as code_file:
            code_file.write(self.pre_code)
            code_file.write("\n")
            code_file.write(code)
            code_file.write("\n")
            code_file.write(self.post_code)

        cmd0 = ["latex", "--interaction=nonstopmode", "tikz.tex"]
        cmd1 = ["dvisvgm", "tikz.dvi"]
        os.chdir(self.get_tmp_dir())
        Popen(cmd0, stdout=os.open(os.devnull, os.O_RDWR), stderr=PIPE).communicate()
        os.chdir(self.get_tmp_dir())
        p = Popen(cmd1, stdout=os.open(os.devnull, os.O_RDWR), stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
            flush_verbatim (self.get_eps())
        else:
            flush_verbatim (err)
