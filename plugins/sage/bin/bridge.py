# Ero Carrera (c) 2004
# ero@dkbza.org
#
# Distributed under GPL.

import os
import tempfile
import traceback
import keyword
import re
import string
import warnings
warnings.simplefilter("ignore") # don't print warnings to stdout
from sage.all import *

__version__='0.8.1'
__author__='Ero Carrera'

DATA_ESCAPE = chr(27)
DATA_COMMAND = chr(16)

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

def data_begin():
	"""Signal the beginning of data to TeXmacs."""
	os.sys.stdout.write(chr(2))

def data_end():
	"""Signal the end of data to TeXmacs."""
	os.sys.stdout.write(chr(5))
	os.sys.stdout.flush()

def texmacs_out(out_str):
	"""Feed data back to TeXmacs.
	
	Output results back to TeXmacs, with the DATA_BEGIN,
	DATA_END control characters."""
	data_begin()
	os.sys.stdout.write(out_str)
	data_end()

def ps_out(ps_file):
	"""Outputs Postscript within TeXmacs.

	According the the type of the argument the following
	scenarios can take place:	

	If the argument is a string and has more than one line, it
	will be processed as raw Postscript data.
	
	If the argument is a string, it's supposed to contain the
	filename of a Postscript file which will be  read ( if the
	file  has no extension, the defaults .ps and .eps will be
	tried.)
	
	If the argument is a file  or other object which provides a
	'read'  method, data will be obtained by calling such
	method.
	
	
	Implemented from suggestion by Alvaro Tejero Cantero.
	Implementation partially based on information provided
	by Mark Arrasmith.
	"""
	
	if 'read' in dir(ps_file):
		data = ps_file.read()
		return chr(2)+'ps:'+data+chr(5)
		
	if ps_file.find('\n')>0:
		return chr(2)+'ps:'+ps_file+chr(5)
	
	ext_list = ['', '.eps', '.ps']
	if isinstance(ps_file, str):
		for ext in ext_list:
			if os.path.exists(ps_file+ext):
				ps_fd = file(ps_file+ext, 'r')
				data = ps_fd.read()
				ps_fd.close()
				break
		else:
			raise IOError('File \''+ps_file+'+'+str(ext_list)+'\' not found.')

	return chr(2)+'ps:'+data+chr(5)

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

			return "ps: "+ps_contents
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
		data2=r''
		for c in data:
			if c not in string.printable:
				data2+='\\x%x' % ord(c)
			else:
				data2+=c
		data=data2

	return 'verbatim: %s' % str(data)

def do_module_hierarchy(mod, attr):
	"""Explore an object's hierarchy.
	
	Go through the objects hierarchy looking for
	attributes/methods to provide as autocompletion
	options.
	"""
	dot = attr.find('.')
 	if dot>0:
		if hasattr(mod, attr[:dot]):
			next = getattr(mod, attr[:dot])
			return do_module_hierarchy(next, attr[dot+1:])
	if isinstance(mod, dict):
		return dir(mod)
	else:
		return dir(mod)

def find_completion_candidates(cmpl_str, my_globals):
	"""Harvest candidates to provide as autocompletion options."""
	
	haystack = my_globals.keys()+dir(my_globals['__builtins__'])+keyword.kwlist
	dot = cmpl_str.rfind('.')
	offset = None
	if dot>0:
		offset = len(cmpl_str[dot+1:])
		first_dot = cmpl_str[:dot].find('.')
		if first_dot<0:
			mod_name = cmpl_str[:dot]
			r_str = cmpl_str[dot+1:]
		else:
			mod_name = cmpl_str[:first_dot]
			r_str = cmpl_str[first_dot+1:]
		if mod_name in keyword.kwlist:
			return None, []
		if os.sys.modules.has_key(mod_name):
			haystack = do_module_hierarchy(os.sys.modules[mod_name], r_str)
		elif mod_name in my_globals.keys():
			haystack = do_module_hierarchy(my_globals[mod_name], r_str)
		else:
			haystack = do_module_hierarchy(type(mod_name), r_str)
			
	return offset, filter(lambda x:x.find(cmpl_str[dot+1:])  ==  0, haystack)

def name_char(c):
	"""Check whether a character is a valid symbol."""
	if c in '+-*/%<>&|^~ = !,:()[]{}':
		return ' '
	else:
		return c

def complete(cmd, my_globals):
	"""Parse autocomplete command.
	 
	Parse the command and return a suitable answer to
	give back to TeXmacs.
	"""

	# Parse Texmacs command and extract string to
	# complete and offset to complete from.
	cmd = cmd.strip()[:-1]
	cmd_re = re.compile(r'"(.*)"\s+(\d+)')
	res = cmd_re.match(cmd)
	
	# if we don't match anything we return
	# no completion possibilities.
	if res is None:
		return 'scheme:(tuple "" "")'
		
	cmpl_str = res.group(1)
	pos_str = int(res.group(2))
	
	cmpl_str = cmpl_str[:pos_str]
	if len(cmpl_str)  ==  0:
		return 'scheme:(tuple "" "")'
	
	# We get the string after the last space character.
	# no completion is done for strings with spaces
	# within
	cmpl_str = str().join(map(name_char, cmpl_str))
	cmpl_str = cmpl_str.split()[-1]
	pos = len(cmpl_str)
	
	# no string after last space? return empty
	# completion
	if len(cmpl_str)  ==  0:
		return 'scheme:(tuple "" "")'
		
	# Find completion candidates and form a suitable
	# answer to Texmacs
	offset, cand = find_completion_candidates(cmpl_str, my_globals)
	if len(cand) == 0:
		res = '""'
	else:
		res = ''
	for c in cand:
		if offset is not None:
			pos = offset
		#Ignore things that start with underscore
		if c[pos] == "_":
			continue
		res += '"%s" ' % c[pos:]
	return 'scheme:(tuple "'+cmpl_str+'" '+res+')'


texmacs_out("verbatim:"+sage.misc.banner.version())

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
co = compile('import __builtin__ as __builtins__', 'tm_sage', 'exec')
eval(co, my_globals)
os.sys.stdout = stdout_saved
co = compile('from sage.all import *', 'tm_sage', 'exec')
eval(co, my_globals)
co = compile('from sage.calculus.predefined import x', 'tm_sage', 'exec')
eval(co, my_globals)




# Main session loop.
while 1:
	line = os.sys.stdin.readline()
	if not line:
		texmacs_out('')
	else:
		if line[0]  ==  DATA_COMMAND:
			if line[1:].find('(complete ')  ==  0:
				texmacs_out(complete(line[11:], my_globals))
			continue
		capt = Capture()
		result = None
		# We guess where the lines will break.
		line = re.sub(r' {2}(\s*)', r'\n \1', line)

		try:
			#Handle the case where the string ends in ??
			if line[-3:] == "??\n":
				result = eval('sage.misc.sageinspect.sage_getsource('+line[:-3]+')', my_globals)
			#Handle the case where the command ends in ?
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
		texmacs_out(out.strip())
