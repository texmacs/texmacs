import os
from io import BytesIO
from io import open
from .protocol import *

class PSOutDummy:
  """ Dummy class for use with ps_out.

  We return an instance of this class to avoid output after 
  evaluation in the TeXmacs plugin of ps_out."""
  
  def __str__(self):
    """Return an empty string for compose_output()"""
    return ''
  def __repr__(self):
    return 'PSOutDummy'

def ps_out(out):
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

  texmacs_out('ps:' + texmacs_escape(data).decode())
  return PSOutDummy()
