#!/usr/bin/env python
"""
*******************************************************************************
* Texmacs extension for Inkscape
* COPYRIGHT  : (C) 2012 Philippe JOYEZ
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************

This piece of code allows to re-edit mathematical equations in WYSIWYG 
way with TeXmacs. It works with equations originally produced by 
versions of TeXmacs <http://www.texmacs.org> dated after july 2012, 
but also with those made by the Textext.ink Inkscape extension
(http://www.iki.fi/pav/software/textext/).

For that, it extracts the TeXmacs (or Latex) description of the equation,
creates on the fly a texmacs file and calls texmacs to edit the equation.
On return, Texmacs has generated an svg file containing the upadted image
and description of the equation, which then replaces the old equation.

If no equation exists when the extension is invoked, then a new
equation is created.

This extension is meant to be as much as possible 2-ways compatible with
Textext.ink equations. Note however that in order to correctly edit texmacs
equations, textext has to be invoked with a preamble file containing the
texmacs latex macros and appropriate includes (see texmacs_latex.sty coming
with this file).

A good deal of the inspiration, implementation and technical details of this
script directly comes from textext.py (http://www.iki.fi/pav/software/textext/)
, an Inkscape extension written by Pauli Virtanen  (pav@iki.fi) that does
the same thing with Latex equations, albeit in an essentially non-WYSIWYG way.
Part of the code is directly copied from textext.py. Thank you Pauli!

The texmacs file we invoke TeXmacs with is a full texmacs document
that contains the equation and a "Click to finish" link triggering a
scheme macro that produces the new version of the svg-texmacs equation.

This codes works together with (*it is useless without*) the said
versions of TeXmacs that are the only ones which generates
the appropriate svg output. 

this files goes into ~/.config/Inkscape/extensions
"""

#------------------------------------------------------------------------------

import sys, os, glob, traceback, platform
import inkex,tempfile, subprocess, copy
from lxml import etree

IS_WINDOWS = (platform.system() == "Windows")
#IS_MACOS= sys.platform.startswith('darwin')

if IS_WINDOWS :
    programfiles = os.environ.get('PROGRAMFILES')
    texmacs_path = os.path.join(programfiles, 'TeXmacs','bin', 'texmacs.exe')
    
else : texmacs_path = 'texmacs' #texmacs needs to be in the path!


TEXTEXT_NS = u"http://www.iki.fi/pav/software/textext/"
TEXMACS_NS = u"http://www.texmacs.org/"
SVG_NS = u"http://www.w3.org/2000/svg"
XLINK_NS = u"http://www.w3.org/1999/xlink"

NSS = {
    u'textext': TEXTEXT_NS,
    u'texmacs': TEXMACS_NS,
    u'svg': SVG_NS,
    u'xlink': XLINK_NS,
}

tm_file_header="<TeXmacs|1.0.7.15>\n\n<style|generic>\n\n<\\body>\n"
tm_dummy_equation="<\equation*>\n    1+1\n  </equation*>\n"
tm_no_equation="\\;\n"
tm_file_trailer="\n\n<label|trailer>When finished editing <action|Click \
here|((go-start)(selection-set-start)(go-to-label \"trailer\")\
(go-left)(go-left)(selection-set-end)(export-selection-as-graphics\
(string-append (url-\>string (current-buffer)) \".svg\"))(quit-TeXmacs))> or \
<action|Cancel|(quit-TeXmacs)>\n</body>"

tmml_template="<?xml version=\"1.0\"?>\n\n<TeXmacs version=\"1.0.7.15\">\n\
  <style><tuple>generic</tuple></style>\n\n  <body>\n <tm-par><insert></insert> </tm-par>\
    <tm-par>\n      <label>trailer</label>When finished editing\
 <action><tm-arg>Click\n      here</tm-arg><tm-arg>((go-start)(selection-set-start)\
(go-to-label\n      \"trailer\")(go-left)(go-left)(selection-set-end)\
(export-selection-as-graphics(string-append\n      (url->string\
 (current-buffer))\n      \".svg\"))(quit-TeXmacs))</tm-arg></action> or\n\
      <action><tm-arg>Cancel</tm-arg><tm-arg>(quit-TeXmacs)</tm-arg></action>\n\
    </tm-par>\n  </body>\n</TeXmacs>"

def exec_command(cmd, ok_return_value=0, combine_error=False):
        """
        Run given command, check return value, and return
        concatenated stdout and stderr.
        """
        try:
            p = subprocess.Popen(cmd,
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE,
                                 stdin=subprocess.PIPE)
            out, err = p.communicate()
        except OSError, e:
            raise RuntimeError("Command %s failed: %s" % (' '.join(cmd), e))
        
        if ok_return_value is not None and p.returncode != ok_return_value:
            raise RuntimeError("Command %s failed (code %d): %s"
                               % (' '.join(cmd), p.returncode, out + err))
        return out + err

def remove_xml_namespace(node, NS):
        ns = '{%s}' % NS
        
        if node.tag.startswith(ns):
            node.tag = node.tag[len(ns):]
        
        for key in node.attrib.keys():
            if key.startswith(ns):
                new_key = key[len(ns):]
                node.attrib[new_key] = node.attrib[key]
                del node.attrib[key]
        
        for c in node:
            remove_xml_namespace(c,NS)

#------------------------------------------------------------------------------
# Inkscape plugin functionality
#------------------------------------------------------------------------------

class Texmacs(inkex.Effect):
    def __init__(self):
        inkex.Effect.__init__(self)
        self.tmp_path = tempfile.mkdtemp()
        self.tmp_base = 'inkscape_edit_tmp.tm'
	self.tmp_name = os.path.join(self.tmp_path,self.tmp_base)

     
    def effect(self):
        """Perform the effect: create/modify TexText objects"""
        
        # Find equation and modify it
        old_node, svgfile = self.get_edit_old()

	if not (svgfile == ""): 
		f = open(svgfile, 'r')
		tree = etree.parse(f)
        	f.close()

		root = tree.getroot()
		new_node = root.find('{%s}g' % SVG_NS)

         	# -- Replace
        	self.replace_node(old_node, new_node)

	#finish : cleanup
        self.remove_temp_files()

    
    def get_edit_old(self):
        """
        Dig out tmtext code save it to temp file and call texmacs to open it.
	upon closing texmacs, if new svg file has been produced

        :Returns: (old_node, returned svg filename or "" if cancelled)
        """

        for i in self.options.ids: 
        # gets list of ids in the call of the extension.
            node = self.selected[i] 
            # selects each member of selection in turn ; 
            #only the first one which can be processed will be
            if node.tag != '{%s}g' % SVG_NS: continue
            
	    elif '{%s}texmacscode'%TEXMACS_NS in node.attrib: # that group contains texmacs data
		
		f_tmp = open(self.tmp_name, 'w') # writes the content of the tmtext string
		tm_file = tm_file_header + \
				node.attrib.get('{%s}texmacscode'%TEXMACS_NS, '').decode('string-escape') +\
				tm_file_trailer
        	try:
 	        	f_tmp.write(tm_file)
        	finally:
            		f_tmp.close()
		exec_command([texmacs_path, self.tmp_name],None,False)
		return (node, self.my_svg_file())	

            elif node.find('.//{%s}TeXmacs' % TEXMACS_NS) != None : 
                #we have tmml (=xml) texmacs data embedded in an svg desc node
                # handle tmml data : insert the tmml equation in a tmml template
                # containing the the re-edition actions scripts and call texmacs with it
                tmnode =copy.deepcopy( node.find('.//{%s}TeXmacs' % TEXMACS_NS))
                remove_xml_namespace(tmnode, TEXMACS_NS )
                tmml = etree.fromstring(tmml_template)
                insert = tmml.find('.//insert') #fake insertion item
                insert_par = insert.getparent() #get the insertion paragraph in the template file
                insert_par[0]= copy.deepcopy(tmnode[0]) #overwrite the fake insertion item
		tmml_file = etree.tostring(tmml, pretty_print=True)
		f_tmp = open(self.tmp_name+"ml", 'w') # writes the content of the tmml text string
		try:
 	        	f_tmp.write(tmml_file)
        	finally:
            		f_tmp.close()
		exec_command([texmacs_path, self.tmp_name+"ml"],None,False)
		svg_name = self.tmp_name + "ml.svg" #if successful texmacs has produced an svg file
	        if not os.path.isfile(svg_name): svg_name = ""
		return (node, svg_name)	

            elif '{%s}text'%TEXTEXT_NS in node.attrib:  ##implements Textext conversion to TeXmacs
		f_tmp = open(self.tmp_name, 'w')
		blank_tm_file = tm_file_header + tm_no_equation + tm_file_trailer
        	try:  # writes the content of the tmtext string

 	        	f_tmp.write(blank_tm_file)
        	finally:
            		f_tmp.close()

		tmcommandopt =  "-x \'(delayed (:idle 000) (insert (latex->texmacs (parse-latex \"\\\\["
		tmcommandopt += node.attrib.get('{%s}text'%TEXTEXT_NS, '') #get Latex code
		tmcommandopt += "\\\\]\"))))\'" 
#-x this option does the actual on the fly conversion of Latex and inserts the result it into the provided blank document
#the actual command called is similar to (square brackets to force full-line 'display' equation):
#/usr/local/bin/texmacs -x '(insert (latex->texmacs (parse-latex "\\[$\\cos \\pi$\\]")))' /tmp/tmpbhpIGQ/tmp.tm
		mycmd='/usr/local/bin/texmacs '+tmcommandopt+' '+self.tmp_name

		subprocess.call([mycmd],shell=True, # need call with shell=True in my linux, otherwise error : file not found...
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE,
                                 stdin=subprocess.PIPE)
		return (node, self.my_svg_file())	


#	if we arrive here *no* editable equation was in selection (including no selection): launch TeXmacs with blank equation.
	f_tmp = open(self.tmp_name, 'w') # writes the content of the tmtext string
	blank_tm_file = tm_file_header + tm_dummy_equation + tm_file_trailer
	try:
 	        f_tmp.write(blank_tm_file)
	finally:
            	f_tmp.close()
	exec_command([texmacs_path, self.tmp_name],None,False)
	return (None, self.my_svg_file())	
                

    def replace_node(self, old_node, new_node):
        """
        Replace an XML node old_node with new_node
        in self.document.
        """
        if old_node is None:
            self.current_layer.append(new_node)
        else:
		# -- Copy transform
        	try:
            	# Note: the new node does *not* have the SVG namespace prefixes!
            	#       This caused some problems as Inkscape couldn't properly
            	#       handle both svg: and prefixless entries in the same file
            	#       in some cases.
        	    new_node.attrib['transform'] = old_node.attrib['transform']
        	except (KeyError, IndexError, TypeError, AttributeError):
        	    pass
        	try:
        	    new_node.attrib['transform'] = old_node.attrib['{%s}transform'%SVG_NS]
        	except (KeyError, IndexError, TypeError, AttributeError):
        	    pass

		# -- Copy style (remembering style is tricky...)
		try:
			new_node.attrib['style'] = old_node.attrib['style']
        	except (KeyError, IndexError, TypeError, AttributeError):
        		pass
		try:
			new_node.attrib['style'] = old_node.attrib['{%s}style'%SVG_NS]
        	except (KeyError, IndexError, TypeError, AttributeError):
        		pass

		# replace node
		parent = old_node.getparent()
        	parent.append(new_node)
		parent.remove(old_node)
            
        # -- Work around probable bugs in several viewers that don't handle
        #    "stroke-width: 0;" style properly.
        style = 'stroke-width: 0.0000001'
        try:
            xstyle = new_node.attrib['style']
        except KeyError:
            try:
                xstyle = new_node.attrib['{%s}style'%SVG_NS]
                del new_node.attrib['{%s}style'%SVG_NS]
            except KeyError:
                xstyle = ""
        if 'stroke-width' not in xstyle:
            if xstyle.strip():
                style = xstyle + ';' + style
        else:
            style = xstyle
        new_node.attrib['style'] = style

	return

    STYLE_ATTRS = ['fill','fill-opacity','fill-rule',
                   'font-size-adjust','font-stretch',
                   'font-style','font-variant',
                   'font-weight','letter-spacing',
                   'stroke','stroke-dasharray',
                   'stroke-linecap','stroke-linejoin',
                   'stroke-miterlimit','stroke-opacity',
                   'text-anchor','word-spacing','style']

    def my_svg_file(self):
	"""returns temporary svg filename if file exists, otherwise empty string"""
	svg_name = self.tmp_name + ".svg" #if successful texmacs has produced an svg file
	if not os.path.isfile(svg_name):
            	svg_name = ""	#otherwise was cancelled (or failed)
	return svg_name

    def remove_temp_files(self):
        """Remove temporary files"""
        base = os.path.join(self.tmp_path, self.tmp_base)
        for filename in glob.glob(base + '*'):
            self.try_remove(filename)
        self.try_remove(self.tmp_path)

    def try_remove(self, filename):
        """Try to remove given file, skipping if not exists."""
        if os.path.isfile(filename):
            os.remove(filename)
        elif os.path.isdir(filename):
            os.rmdir(filename)


#------------------------------------------------------------------------------
# Entry point
#------------------------------------------------------------------------------

if __name__ == "__main__":
    e = Texmacs()
    e.affect()
