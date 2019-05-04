#!/usr/bin/env python
###############################################################################
##
## MODULE      : protocol.py
## DESCRIPTION : The TeXmacs plugin protocol impl
## COPYRIGHT   : (C) 2004  Ero Carrera, ero@dkbza.org
##               (C) 2012  Adrian Soto
##               (C) 2014  Miguel de Benito Delgado, mdbenito@texmacs.org
##               (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os

DATA_BEGIN = chr(2)
DATA_END = chr(5)
DATA_ESCAPE = chr(27)
DATA_COMMAND = chr(16)


def data_begin():
    """Signal the beginning of data to TeXmacs."""
    os.sys.stdout.write(chr(2))


def data_end():
    """Signal the end of data to TeXmacs."""
    os.sys.stdout.write(chr(5))
    os.sys.stdout.flush()


def texmacs_escape(data):
    return data.replace(DATA_BEGIN.encode(), (DATA_ESCAPE + DATA_BEGIN).encode()) \
               .replace(DATA_END.encode(), (DATA_ESCAPE + DATA_END).encode())


def texmacs_out(out_str):
    """Feed data back to TeXmacs.

    Output results back to TeXmacs, with the DATA_BEGIN,
    DATA_END control characters."""
    data_begin()
    os.sys.stdout.write(out_str)
    data_end()
