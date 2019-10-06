#!/usr/bin/env python
###############################################################################
##
## MODULE      : dratex.py
## DESCRIPTION : DraTeX support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .latex import LaTeX
from ..protocol import *


class DraTeX(LaTeX):
    def __init__(self, name = "dratex"):
        super(DraTeX, self).__init__()
        self.name = name
        the_plugin_path = get_plugin_path(name)
        self.pre_code = """
\\documentclass{standalone}
\\input %s/latex/DraTex.sty
\\input %s/latex/AlDraTex.sty
\\pagestyle{empty}
\\begin{document}
""" % (the_plugin_path, the_plugin_path)
        self.post_code = """
\\end{document}
"""
        self.message = "TeXmacs interface to (Al)DraTex (High Level Drawing Facilities)"

    def evaluate(self, code):
        if not (code.lstrip().startswith("\\documentclass")):
            code = self.pre_code + "\n" + code + "\n" + self.post_code

        super(DraTeX, self).evaluate(code)

