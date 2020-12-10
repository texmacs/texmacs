#!/usr/bin/env python
###############################################################################
##
## MODULE      : postscript.py
## DESCRIPTION : Generate PostScript/PDF within GNU TeXmacs
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
import platform
from io import BytesIO
from io import open
from .protocol import *


class PSOutDummy:
    """ Dummy class for use with ps_out.

    We return an instance of this class to avoid output after 
    evaluation in the TeXmacs plugin of ps_out."""

    def __init__(self, data):
        self.content = data

    def __str__(self):
        """Return an empty string for compose_output()"""
        return ''

    def __repr__(self):
        return 'PSOutDummy'

class FileOutDummy:
    """ Python object to feed a file back to TeXmacs. """

    def __init__(self, data):
        self.content = data

    def __str__(self):
        """Return an empty string for compose_output()"""
        return ''

    def __repr__(self):
        return 'FileOutDummy'


def ps_out (out):
    """Outputs PostScript within TeXmacs.

    According the the type of the argument the following
    scenarios can take place:    

    If the argument is an instance of matplotlib.pyplot.Figure
    then its method savefig() will be used to produce an EPS
    figure. Note that you need to be using a backend which
    supports this format.

    If the argument is a string and has more than one line, it
    will be processed as raw Postscript data.

    If the argument is a string with no line breaks, it is assumed
    to contain the filename of a Postscript file which will be
    read (if the file  has no extension, the defaults .eps and .ps
    will be tried in this order).

    If the argument is a file or any other object which provides
    a 'read'  method, data will be obtained by calling such
    method.

    Implemented from suggestion by Alvaro Tejero Cantero.
    Implementation partially based on information provided
    by Mark Arrasmith.
    """
    if 'savefig' in dir(out):
        str_out = BytesIO()
        out.savefig(str_out, format='eps')
        data = str_out.getvalue()
        str_out.close()
    elif isinstance(out, str):
        if out.find('\n') > 0:
            data = out
        else:
            ext_list = ['', '.eps', '.ps']
        for ext in ext_list:
            if os.path.exists(out+ext):
                fd = open(out+ext, 'rb')
                data = fd.read()
                fd.close()
                break
            else:
                raise IOError('File "%s%s" not found.' % (out, str(ext_lis)))
    elif 'read' in dir(out):
        data = out.read()

    return PSOutDummy(texmacs_escape(data).decode())

pdf_out_tmp_file = "pdf_out_" + str(os.getpid()) + ".pdf"
if (platform.system() == "Windows"):
    pdf_out_tmp_file = os.getenv("TEXMACS_HOME_PATH") + "\\system\\tmp\\" + pdf_out_tmp_file
else:
    pdf_out_tmp_file = os.getenv("TEXMACS_HOME_PATH") + "/system/tmp/" +  pdf_out_tmp_file


def pdf_out (out):
    """Outputs PDF within TeXmacs.

    According the the type of the argument the following
    scenarios can take place:

    If the argument is an instance of matplotlib.pyplot.Figure
    then its method savefig() will be used to produce a PDF
    figure.
    Note that you need to be using a backend which supports this format.

    If the argument is a string with no line breaks, it is assumed
    to contain the filename of a PDF file which will be
    passed to TeXmacs (if the file  has no extension, the default
    .pdf will be tried).

    If the argument is a file or any other object which provides
    a 'read'  method, data will be obtained by calling such
    method.
    
    In any case a (maybe temporary) PDF filename is passed to TeXmacs.
    Adapted from ps_out().
    """

    if 'savefig' in dir(out):
        out.savefig(pdf_out_tmp_file, format='pdf')
        name = pdf_out_tmp_file
    elif isinstance(out, str):
        if out.find('\n') > 0:
            name = None
        else:
            ext_list = ['', '.pdf']
        for ext in ext_list:
            if os.path.exists(out+ext):
                name = out+ext
                break
            else:
                raise IOError('File "%s%s" not found.' % (out, str(ext_lis)))
    elif 'read' in dir(out):
        data = out.read()
        fd = open(pdf_out_tmp_file, 'wb')
        fd.write(data)
        fd.close()
        name = pdf_out_tmp_file

    return FileOutDummy(name)
