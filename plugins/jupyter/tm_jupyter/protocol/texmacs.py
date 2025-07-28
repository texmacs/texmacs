###############################################################################
##
## MODULE      : protocol.py
## DESCRIPTION : The TeXmacs plugin protocol impl
## COPYRIGHT   : (C) 2004  Ero Carrera, ero@dkbza.org
##               (C) 2012  Adrian Soto
##               (C) 2014  Miguel de Benito Delgado, mdbenito@texmacs.org
##               (C) 2019  Darcy Shen
##               (C) 2021  Jeroen Wouters
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import sys, os
#import fcntl
import io
from contextlib import contextmanager, redirect_stderr, redirect_stdout
import builtins

import re
ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')

DATA_BEGIN = chr(2)
DATA_END = chr(5)
DATA_ESCAPE = chr(27)
DATA_COMMAND = chr(16)

def in_texmacs():
    return (os.environ.get("TEXMACS_PATH") != None)

def data_begin():
    """Signal the beginning of data to TeXmacs."""
    sys.stdout.write(DATA_BEGIN if in_texmacs() else "DATA_BEGIN")

def data_end():
    """Signal the end of data to TeXmacs."""
    sys.stdout.write(DATA_END if in_texmacs() else "DATA_END")
    sys.stdout.flush()

# This seems to fix BlockingIOError issues with large images...
class UnblockTTY:
    def __enter__(self):
        self.fd = sys.stdout.fileno()
        self.block_save = os.get_blocking(self.fd)
        os.set_blocking(self.fd, True)
        #self.flags_save = fcntl.fcntl(self.fd, fcntl.F_GETFL)
        # flags = self.flags_save & ~os.O_NONBLOCK
        #fcntl.fcntl(self.fd, fcntl.F_SETFL, flags)

    def __exit__(self, *args):
        #fcntl.fcntl(self.fd, fcntl.F_SETFL, self.flags_save)
        os.set_blocking(self.fd, self.block_save)

@contextmanager
def escape_context(filter_ansi=False):
    out_buf = io.StringIO()
    err_buf = io.StringIO()
    with redirect_stdout(out_buf), redirect_stderr(err_buf):
        yield
    if filter_ansi:
        sys.stdout.write(_escape(ansi_escape.sub('',out_buf.getvalue())))
        sys.stderr.write(_escape(ansi_escape.sub('',err_buf.getvalue())))
    else:
        sys.stdout.write(_escape(out_buf.getvalue()))
        sys.stderr.write(_escape(err_buf.getvalue()))

@contextmanager
def flush(type,escape=True):
    # possible `type` values include: 
    # verbatim, utf8, command, scheme, latex, file, ps, html, math, bibtex, texmacs,
    # and any format that has a "parse-XXXX-snippet" function.
    # see src/Data/Convert/Generic/input.cpp
    with UnblockTTY():
      data_begin()
      sys.stdout.write(type + (":" if type != "prompt" else "#"))
      if escape and in_texmacs():
        if (type == "verbatim") or (type == "utf8"):
            with escape_context(filter_ansi=True):
              yield
        else:
            with escape_context():
              yield
      else:
        yield
      data_end()

def _escape(content):
    return content.replace(DATA_ESCAPE, DATA_ESCAPE + DATA_ESCAPE) \
               .replace(DATA_BEGIN, DATA_ESCAPE + DATA_BEGIN) \
               .replace(DATA_END, DATA_ESCAPE + DATA_END)
