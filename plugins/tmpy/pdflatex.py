#!/usr/bin/env python
###############################################################################
##
## MODULE      : pdflatex.py
## DESCRIPTION : LaTeX graphics support
## COPYRIGHT   : (C) 2019  Darcy Shen and Massimiliano Gubinelli
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from .compat import which
from .protocol import *


class PDFLaTeX(Graph):
    def __init__(self, name = "pdflatex"):
        super(PDFLaTeX, self).__init__()
        self.name = name
        self.message = "TeXmacs interface to PDFLaTeX for picture generation"

    def available(self):
        return all(list(map(lambda x: which(x) is not None, ["pdflatex"])))

    def evaluate(self, code):

        if not (code.lstrip().startswith("\\documentclass")):
            flush_err("I expect a valid LaTeX document, usually starting with\n"
                +"   \\documentclass{standalone}\n"
                +"which will take proper care of suitable bounding box for the image.\n"
                +"The document is sent to latex and then to dvips to produce a single image.")    
            flush_verbatim("")
            return

        code_path = self.get_tmp_dir() + self.name + ".tex"
        pdf_path = self.get_tmp_dir() + self.name + ".pdf"

        with open(code_path, 'w') as code_file:
            code_file.write(code)

        cmd0 = ["pdflatex", "--interaction=errorstopmode", "-halt-on-error", code_path]

        # flush_err (code) # to debug
        phase = "PDFLaTeX"
        #flush_err ("Running " + phase)
        os.chdir(self.get_tmp_dir())
        p = Popen(cmd0, stdout=PIPE, stderr=STDOUT)
        out, err = p.communicate()
        if (p.returncode == 0):
            phase = "Finished"
            #flush_err ("Running " + phase)
            flush_file (pdf_path)
            return
        flush_err (phase + " error!\n" + out.decode())
        flush_verbatim("")   # otherwise TeXmacs keeps waiting output
            
