import os
from subprocess import Popen, PIPE, STDOUT
from .graph     import Graph
from ..protocol import *
from ..compat   import *

class Gnuplot(Graph):
    def __init__(self, name = "gnuplot"):
        super(Gnuplot, self).__init__()
        self.name = name
        self.width = 600
        self.pre_code = """
reset
set terminal postscript eps enhanced
set output 
set output '%s'
set size 1,1
set autoscale
""" % (self.get_eps_path())

    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen([self.name, "-V"], stdout=PIPE)
                ret, err = p.communicate()
                # WARN: The Version Info is in stderr
                if (p.returncode == 0):
                    self.message = ret.decode()
            except OSError:
                pass
        super(Gnuplot, self).greet()

    def evaluate(self, code):
        code_path = self.get_tmp_dir() + self.name + ".txt"
        with open(code_path, 'w') as code_file:
            code_file.write(self.pre_code)
            code_file.write(code)

        cmd = [self.name, "-c", code_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
          flush_file (self.get_eps())
        else:
          flush_verbatim (err.decode())

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
                lines = []
                for x in line.split('~'):
                    lines.append(x)
                while line != "<EOF>":
                    line = tm_input()
                    for x in line.split('~'):
                        lines.append(x)
                text = '\n'.join(lines[:-1])
                self.eval(text)

