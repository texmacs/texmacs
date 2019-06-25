#!/usr/bin/env python
###############################################################################
##
## MODULE      : xypic.py
## DESCRIPTION : XYpic plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##               (C) 2004 Nicolas Ratier (nicolas DOT ratier AT lpmo DOT edu)
##               (C) XYpic latex package Kristoffer H. Rose
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .latex import LaTeX
from ..protocol import *


class XYpic(LaTeX):
    def __init__(self, name = "xypic"):
        super(XYpic, self).__init__()
        self.name = name

        self.pre_code = """
\\documentclass{standalone}
\\usepackage[all]{xy}
\\begin{document}
"""
        self.post_code = "\end{document}"
        self.message = "TeXmacs interface to XYpic (high level 2-dimensional graphics)"

    def evaluate(self, code):
        if not (code.lstrip().startswith("\\documentclass")):
            code = self.pre_code + "\n" + code + "\n" + self.post_code

        super(XYpic, self).evaluate(code)

