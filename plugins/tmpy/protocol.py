#!/usr/bin/env python3
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

import os

DATA_BEGIN = chr(2)
DATA_END = chr(5)
DATA_ESCAPE = chr(27)
DATA_COMMAND = chr(16)


def data_begin():
    """Signal the beginning of data to TeXmacs."""
    os.sys.stdout.write(DATA_BEGIN)


def data_end():
    """Signal the end of data to TeXmacs."""
    os.sys.stdout.write(DATA_END)
    os.sys.stdout.flush()


def texmacs_escape(data):
    return data.replace(DATA_ESCAPE, DATA_ESCAPE + DATA_ESCAPE) \
               .replace(DATA_BEGIN, DATA_ESCAPE + DATA_BEGIN) \
               .replace(DATA_END, DATA_ESCAPE + DATA_END)


def flush_any (out_str):
    """Feed data back to TeXmacs.

    Output results back to TeXmacs, with the DATA_BEGIN,
    DATA_END control characters."""
    data_begin()
    os.sys.stdout.write(texmacs_escape(out_str))
    data_end()

# see src/Data/Convert/Generic/input.cpp

def flush_verbatim(content):
    flush_any ("verbatim:" + content)
    
def flush_utf8(content):
    flush_any ("utf8:" + content)

def flush_newline (n = 1):
    if (n >= 1):
        for i in range(n):
            flush_verbatim("\n")

def flush_prompt(prompt):
    flush_any ("prompt#" + prompt)

def flush_command(command):
    flush_any ("command:" + command)

def flush_scheme(scheme):
    flush_any ("scheme:" + scheme)

def flush_latex(latex):
    flush_any ("latex:$" + latex + "$")

def flush_file(path):
    flush_any ("file:" + path)

def flush_ps(content):
    flush_any ("ps:" + content)

def flush_html(content):
    flush_any ("html:" + content)

def flush_math(content):
    flush_any ("math:" + content)

def flush_bibtex(content):
    flush_any ("bibtex:" + content)

def flush_texmacs(content):
    flush_any ("texmacs:" + content)

def flush_xformat(content,format):
    # XFORMAT accepts any format that has a "parse-XXXX-snippet" function
    # This could be defined in TeXmacs by the plugin using flush_scheme
    flush_any (format + ":" + content)

def flush_err(content):
    os.sys.stderr.write(DATA_BEGIN)
    os.sys.stderr.write("verbatim:" + content)
    os.sys.stderr.write(DATA_END)
    os.sys.stderr.flush() 
