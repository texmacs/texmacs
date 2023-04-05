#!/usr/bin/env python
###############################################################################
##
## MODULE      : graph.py
## DESCRIPTION : An abstract Graph class
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import platform
import shutil
import time
from ..protocol import *
from ..compat import *


class Graph(object):
    name = ""
    message = ""
    pre_code = ""
    post_code = ""
    height = ""
    width = ""
    output = ""
    default_width = "0px"
    default_height = "0px"
    default_output = "eps"

    def greet(self):
        for x in self.message.split("\n"):
            if len(x) == 0:
                pass
            else:
                flush_verbatim (x + "\n")
                flush_prompt (self.name + "] ")

    def available(self):
        return which(self.name) is not None

    def reset_options(self):
        self.height = self.default_height
        self.width = self.default_width
        self.output = self.default_output

    def apply_magic(self, magic_line):
        args = list(filter(lambda x: len(x)!=0, magic_line.split(" ")[1:]))
        while (len(args) > 1):
            option = args[0]
            value = args[1]
            if (option == '-width'):
                self.width = value
            elif (option == '-height'):
                self.height = value
            elif (option == '-output'):
                self.output = value
            else:
                pass
            args = args[2:]

    def before_evaluate(self):
        self.reset_options()
        if not os.path.exists(self.get_tmp_dir()):
            os.mkdir(self.get_tmp_dir())

    def evaluate(self, code):
        pass

    def after_evaluate(self):
        self.clean_tmp_dir()
        self.remove_tmp_dir()

    def eval(self, code):
        self.before_evaluate()
        if (code.startswith("%")):
            magic_lines = code.split("\n")
            magic_line = magic_lines[0]
            self.apply_magic(magic_line)
            code = '\n'.join(magic_lines[1:])
        self.evaluate(code)
        time.sleep(1)
        self.after_evaluate()

    def main_loop(self):
        # Main session loop.
        while True:
            line = tm_input()
            if not line:
                continue
            if line[0] == DATA_COMMAND:
                # TODO: Handle completions
                continue
            else:
                lines = [line]
                while line != "<EOF>":
                    line = tm_input()
                    lines.append(line)
                text = '\n'.join(lines[:-1])
                self.eval(text)

    def get_tmp_dir(self):
        dir = "graph_" + self.name + "_" + str(os.getpid())
        if (platform.system() == "Windows"):
            return os.getenv("TEXMACS_HOME_PATH") + "\\system\\tmp\\" + dir + "\\"
        else:
            return os.getenv("TEXMACS_HOME_PATH") + "/system/tmp/" + dir + "/"

    def remove_tmp_dir(self):
        if (platform.system() != "Windows"):
            os.rmdir(self.get_tmp_dir())

    def clean_tmp_dir(self):
        folder = self.get_tmp_dir()
        for the_file in os.listdir(folder):
            file_path = os.path.join(folder, the_file)
            try:
                if os.path.isfile(file_path):
                    os.unlink(file_path)
                elif os.path.isdir(file_path):
                    shutil.rmtree(file_path)
            except Exception as e:
                print(e)

    def get_png_path(self):
        return self.get_tmp_dir() + self.name + ".png"

    def get_eps_path(self):
        return self.get_tmp_dir() + self.name + ".eps"

    def get_svg_path(self):
        return self.get_tmp_dir() + self.name + ".svg"

    def get_png(self):
        return self.get_png_path() +\
            "?" + "width=" + str(self.width) +\
            "&" + "height=" + str(self.height)

    def get_eps(self):
        return self.get_eps_path() +\
            "?" + "width=" + str(self.width) +\
            "&" + "height=" + str(self.height)

    def get_svg(self):
        return self.get_svg_path() +\
            "?" + "width=" + str(self.width) +\
            "&" + "height=" + str(self.height)
