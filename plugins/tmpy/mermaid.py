#!/usr/bin/env python
###############################################################################
##
## MODULE      : mermaid.py
## DESCRIPTION : Mermaid plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from subprocess import Popen, PIPE, STDOUT
from .graph import Graph
from .protocol import *
from .compat import which

class Mermaid(Graph):
    def __init__(self, name = "mmdc"):
        super(Mermaid, self).__init__()
        self.name = name

    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen([self.name, "--version"], stdout=PIPE)
                ret, err = p.communicate()
                if (p.returncode == 0):
                    self.message = "Mermaid " + ret.decode()
            except OSError:
                pass
        super(Mermaid, self).greet()

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".mmd"
        with open(code_path, 'w') as code_file:
            code_file.write(code)

        puppeteer_config = self.get_tmp_dir() + "puppeteer-config.json"
        with open(puppeteer_config, "w") as config_file:
            config_file.write('{"args": ["--no-sandbox"]}')

        png_path = self.get_png_path()
        if os.path.isfile (png_path):
            os.remove (png_path)
        cmd = [self.name, "-p", puppeteer_config, "-i", code_path, "-o", png_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
            flush_file (self.get_png())
        else:
            flush_verbatim (err.decode())
