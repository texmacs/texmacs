#!/usr/bin/env python
###############################################################################
##
## MODULE      : feynmf.py
## DESCRIPTION : FeynMF graphics support
## COPYRIGHT   : (C) 2019  Darcy Shen and Massimiliano Gubinelli
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

## Inspired by the tm_feynmf pluging written by M. R. Wegewijs 

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from ..compat import which
from ..protocol import *

# we cannot derive from the LaTeX object since we need more complex processing 
# than latex->dvips

class FeynMF(Graph):
    def __init__(self, name = "feynmf"):
        super(FeynMF, self).__init__()
        self.name = name
        self.message = "TeXmacs interface to the FeynMF picture generator"

    def available(self):
        return all(list(map(lambda x: which(x) is not None, ["latex", "dvips", "mpost", "gs"])))

    def evaluate(self, code):

        if not (code.lstrip().startswith("\\documentclass")):
            code = ("\\documentclass{article}\n" ## feynmf does not work with the standalone class
                    "\\usepackage{feynmp}\n"
                    "\\pagestyle{empty}\n"    
                    "\\unitlength=20mm\n"
                    "\\begin{document}\n" 
                    "\\begin{fmffile}{fmftempl}\n"   
                    # add syles    
                    "\\begin{fmfgraph*}"        
                    ) + code + "\n\\end{fmfgraph*}\n\\end{fmffile}\n\\end{document}\n"   

        code_path = self.get_tmp_dir() + self.name + ".tex"
        dvi_path = self.get_tmp_dir() + self.name + ".dvi"
        ps2_path = self.get_tmp_dir() + self.name + ".ps"

        with open(code_path, 'w') as code_file:
            code_file.write(code)

        cmd0 = ["latex", "--interaction=nonstopmode", "-halt-on-error", code_path ] 
        cmd1 = ["dvips", "-q", dvi_path, "-o", ps2_path ]
        cmd2 = ["mpost", "--interaction=nonstopmode", "-halt-on-error", "fmftempl.mp" ]
#        cmd2 = ["mf", "--interaction=nonstopmode", "-halt-on-error", "\mode:=localfont; input fmftempl" ]
        cmd3 = ["gs", "-o", self.get_eps_path(), "-sDEVICE=eps2write" , "-dEPSCrop" , ps2_path ]

        # flush_err (code) # to debug
        phase = "LaTeX (pass 1/2)"    
        #flush_err ("Running " + phase)
        os.chdir(self.get_tmp_dir())
        p = Popen(cmd0, stdout=PIPE, stderr=STDOUT)
        out, err = p.communicate()
        if (0 == 0): # (p.returncode == 0):
            phase = "MetaPost"    
            #flush_err ("Running " + phase)
            p = Popen(cmd2, stdout=PIPE, stderr=STDOUT)
            out, err = p.communicate()
            if (p.returncode == 0):
                phase = "LaTeX (pass 2/2)"    
                #flush_err ("Running " + phase)
                p = Popen(cmd0, stdout=PIPE, stderr=STDOUT)
                out, err = p.communicate()
                if (p.returncode == 0):
                    phase = "dvips"    
                    #flush_err ("Running " + phase)
                    p = Popen(cmd1, stdout=PIPE, stderr=STDOUT)
                    out, err = p.communicate()
                    if (p.returncode == 0):
                        phase = "Distilling EPS"    
                        #flush_err ("Running " + phase)
                        p = Popen(cmd3, stdout=PIPE, stderr=STDOUT)
                        out, err = p.communicate()
                        if (p.returncode == 0):
                            phase = "Finished"    
                            #flush_err ("Running " + phase)
                            flush_file (self.get_eps())
                            return
        flush_err (phase + " error!\n" + out.decode())
        flush_verbatim("")   # otherwise TeXmacs keeps waiting output
