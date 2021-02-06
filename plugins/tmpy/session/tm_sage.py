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
tmpy_home_path = os.environ.get("TEXMACS_HOME_PATH") + "/plugins/tmpy"
if (exists (tmpy_home_path)):
    sys.path.append(os.environ.get("TEXMACS_HOME_PATH") + "/plugins/")
else:
    sys.path.append(os.environ.get("TEXMACS_PATH") + "/plugins/")


import tempfile
import traceback
import re
import string
import ast
import warnings
warnings.simplefilter("ignore") # don't print warnings to stdout
from tmpy.compat import py_ver, tm_input
from tmpy.capture import CaptureStdout
from tmpy.postscript import ps_out, PSOutDummy, pdf_out, FileOutDummy
from tmpy.completion import parse_complete_command, complete
from tmpy.protocol   import *
from sage.all        import *

if py_ver == 2:
    flush_err ("This version of SageMath uses Python 2, which is no longer actively supported in TeXmacs.\nIf you experience any problems please upgrade to SageMath 9.0 or higher.")
#    exit (-1)

# import logging as log
# log.basicConfig(filename='/tmp/tm_python.log',level=log.INFO)

def flush_output (data):
# TODO: integrate Sage to TeXmacs parsing from compose_output()
    """Do some parsing on the output according to its type.
    
    Non printable characters in unicode strings are escaped
    and objects of type None are not printed (so that procedure calls,
    as opposed to function calls, don't produce any output)."""

    if (data is None):
        flush_verbatim ("")
        return

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

            flush_ps(ps_contents)
        except:
            pass
    elif isinstance(data, SageObject):
        try:
            #Replace latex arrays with matrix
            flush_latex(latex(data))
        except:
            flush_verbatim('%s' % str(data))
    elif isinstance(data, str):
        flush_verbatim(data.strip())
    elif isinstance(data, int):
        flush_verbatim('%d' % data)
    elif isinstance(data, float):
        flush_verbatim('%f' % data)
    elif isinstance (data, PSOutDummy):
        flush_ps (data.content)
    elif isinstance (data, FileOutDummy):
        if (data.content is None):
            flush_verbatim ("")
        else:
            flush_file (data.content)
    else:
        flush_verbatim (str(data).strip())

def my_eval (code, p_globals):
    '''Execute a script and return the value of the last expression'''

    block = ast.parse(code, mode='exec')
    if len(block.body) > 1 and isinstance(block.body[-1], ast.Expr):
        last = ast.Expression(block.body.pop().value)
        exec(compile(block, '<string>', mode='exec'), p_globals)
        return eval(compile(last, '<string>', mode='eval'), p_globals)
    else:
        return eval(code, p_globals)

__version__ = '0.8.1'
__author__ = 'Ero Carrera'
my_globals   = {}

# We insert into the session's namespace the 'ps_out' method.
my_globals['ps_out'] = ps_out
my_globals['pdf_out'] = pdf_out

# As well as some documentation.
my_globals['__doc__'] = """A SageMath plugin for TeXmacs.

  TeXmacs SAGE interface v0.8.1.

  Based on the TeXmacs Python plugin by Ero Carrera (c) 2004
  
  The version distributed with TeXmacs is always the latest.

  Enjoy it!
  """

if py_ver > 3:
    text = 'import builtins as __builtins__'
else:
    text = 'import __builtin__ as __builtins__'
CaptureStdout.capture (text, my_globals, "tm_sage")

sys.stdout = os.fdopen (sys.stdout.fileno(), 'w')

co = compile('from sage.all import *', 'tm_sage', 'exec')
eval(co, my_globals)
co = compile('from sage.calculus.predefined import x', 'tm_sage', 'exec')
eval(co, my_globals)

###############################################################################
# Session start
###############################################################################
if (os.path.exists (tmpy_home_path)):
    flush_verbatim ("WARNING: You are under develop mode using " + tmpy_home_path)
    flush_newline (2)
flush_verbatim (sage.misc.banner.version() + "\n" +
		"Python " + sys.version + "\n" +
               "SageMath plugin for TeXmacs.\n" +
               "Please see the documentation in Help -> Plugins -> Sage")
flush_prompt (">>> ")
while True:
    line= tm_input ()
    if not line:
        continue
    if line[0] == DATA_COMMAND:
        sf = parse_complete_command (line[1:])
        if sf[0] == 'complete':
            flush_scheme (complete (sf[1], sf[2], my_globals))
        continue
    elif line.endswith('??') and not line.strip().startswith('#'):
        result = eval('sage.misc.sageinspect.sage_getsource('+line[:-2]+')', my_globals)
    elif line.endswith('?') and not line.strip().startswith('#'):
        if len(line) > 1:
            result = eval('sage.misc.sageinspect.sage_getdoc('+line[:-1]+')', my_globals)
        else:
            flush_verbatim ('Type a name before the "?" to see the help')
        continue
    else:
        lines= [line]
        while line != "<EOF>":
            line= tm_input ()
            if line == '': 
                continue
            lines.append (line)
        text = '\n'.join (lines[:-1])
        try: # Is it an expression?
            result= my_eval (preparse(text), my_globals)
        except:
            result= CaptureStdout.capture (preparse(text), my_globals, "tm_sage")
        flush_output (result)
