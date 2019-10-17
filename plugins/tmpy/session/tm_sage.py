#!/usr/bin/env python
###############################################################################
#
# MODULE      : tm_sage.py
# DESCRIPTION : Initialize python plugin
# COPYRIGHT   : (C) 2004  Ero Carrera, ero@dkbza.org
#               (C) 2019  Darcy Shen
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import sys
from os.path import exists
if (exists (os.environ.get("TEXMACS_HOME_PATH"))):
    sys.path.append(os.environ.get("TEXMACS_HOME_PATH") + "/plugins/")
else:
    sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")

import tempfile
import traceback
import re
import string
import warnings
warnings.simplefilter("ignore") # don't print warnings to stdout
from sage.all import *
from tmpy.protocol import *
from tmpy.postscript import ps_out
from tmpy.completion import parse_complete_command, complete

__version__ = '0.8.1'
__author__ = 'Ero Carrera'

class Capture:
    """Capture python output.
  
    Class in charge of recording the output of the
    statements/expressions entered in the TeXmacs
    session and executed in Python.
    """
    def __init__(self):
        self.text = ''
    def write(self, str):
        self.text += str
    def getOutput(self):
        return self.text
    def flush(self):
        self.text = ''

def compose_output(data):
    """Do some parsing on the output according to its type."""

    if data == None:
        return "verbatim: "

    #If the object is a graphics object, try to return
    #postscript to TeXmacs
    if isinstance(data, Graphics):
        try:
            #Save it to a file
            filename = tempfile.mktemp(suffix='.ps')
            data.save(filename)

            #Read the file
            ps_file = open(filename)
            ps_contents = ps_file.read()
            ps_file.close()

            return "ps: " + ps_contents
        except:
            pass

    if isinstance(data, SageObject):
        try:
            l = latex(data)
            #Replace latex arrays with matrix
            return "latex:" + "$" +  l + "$"
        except:
          return "verbatim: %s" % str(data)
    if isinstance(data, str):
        return 'verbatim:'+data.strip()
    if isinstance(data, int):
        return 'verbatim: %d' % data
    if isinstance(data, float):
        return 'verbatim: %f' % data
  
    if isinstance(data, unicode):
        data2 = r''
        for c in data:
            if c not in string.printable:
                data2 += '\\x%x' % ord(c)
            else:
                data2 += c
        data = data2

    return 'verbatim: %s' % str(data)


flush_verbatim (sage.misc.banner.version())

my_globals = {}
# We insert into the session's namespace the 'ps_out' method.
my_globals['ps_out'] = ps_out

# As well as some documentation.
my_globals['__doc__'] = """TeXmacs SAGE plugin.

  TeXmacs SAGE interface v0.8.1.

  Based on the TeXmacs Python plugin by Ero Carrera (c) 2004
  
  The version distributed with TeXmacs is always the latest.

  Enjoy it!
  """

capt = Capture()
os.chdir( os.environ['HOME'] + '/.TeXmacs/system/tmp')
stdout_saved, os.sys.stdout  =  os.sys.stdout, capt
if os.sys.version[0] == '2':
    co = compile('import __builtin__ as __builtins__', 'tm_sage', 'exec')
else:
    co = compile('import builtins', 'tm_sage', 'exec')
eval(co, my_globals)
os.sys.stdout = stdout_saved
co = compile('from sage.all import *', 'tm_sage', 'exec')
eval(co, my_globals)
co = compile('from sage.calculus.predefined import x', 'tm_sage', 'exec')
eval(co, my_globals)


# Main session loop.
while True:
    line = os.sys.stdin.readline()
    if not line:
        flush_any ('')
    else:
        if line[0]  ==  DATA_COMMAND:
            sf = parse_complete_command (line[1:])
            if sf[0] == 'complete':
                flush_scheme (complete (sf[1], sf[2], my_globals))
            continue
        capt = Capture()
        result = None
        # We guess where the lines will break.
        line = re.sub(r' {2}(\s*)', r'\n \1', line)

        try:
            # Handle the case where the string ends in ??
            if line[-3:] == "??\n":
                result = eval('sage.misc.sageinspect.sage_getsource('+line[:-3]+')', my_globals)
            # Handle the case where the command ends in ?
            elif line[-2:] == "?\n":
                result = eval('sage.misc.sageinspect.sage_getdoc('+line[:-2]+')', my_globals)
            else:
                out = eval(preparse(line), my_globals)
                result = out
        except:
            try:
                stdout_saved, os.sys.stdout  =  os.sys.stdout, capt
                co = compile(preparse(line), 'tm_sage', 'exec')
                eval(co, my_globals)
                os.sys.stdout = stdout_saved
                result = capt.getOutput()
            except Exception:
                traceback.print_exc(file = os.sys.stdout, limit = 0)
                os.sys.stdout = stdout_saved
                result = capt.getOutput()
        del capt
      
        out = compose_output(result)
        flush_any (out.strip())
