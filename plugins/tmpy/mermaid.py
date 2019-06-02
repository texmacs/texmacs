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

class Mermaid(Graph):
    def __init__(self, name = "mmdc"):
        super(Mermaid, self).__init__()
        self.name = name
        try:
            p = Popen([self.name, "--version"], stdout=PIPE)
            ret, err = p.communicate()
            if (p.returncode == 0):
                self.message = "Mermaid " + ret.decode()
        except OSError:
            pass

    def evaluate(self, code):
        code_path = os.getenv("TEXMACS_HOME_PATH") +\
            "/system/tmp/" + self.name + ".mmd"
        with open(code_path, 'w') as code_file:
            code_file.write(code)

        puppeteer_config = os.getenv("TEXMACS_HOME_PATH") +\
            "/system/tmp/puppeteer-config.json"
        with open(puppeteer_config, "w") as config_file:
            config_file.write('{"args": ["--no-sandbox"]}')

        png = self.get_png_path()
        png_path = png.split("?")[0]
        cmd = [self.name, "-p", puppeteer_config, "-i", code_path, "-o", png_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
            flush_file (png)
        else:
            flush_verbatim (err.decode())
