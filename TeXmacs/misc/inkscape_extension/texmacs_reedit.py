#!/usr/bin/env python
"""
*******************************************************************************
* Texmacs extension for Inkscape
* COPYRIGHT  : (C) 2012 Philippe JOYEZ and the TeXmacs team
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
this files goes into ~/.config/Inkscape/extensions

This extension allows to (re-)edit mathematical equations in WYSIWYG 
way with TeXmacs. It works with equations originally produced by 
versions of TeXmacs <https://www.texmacs.org> dated after july 2012, 
but also with those made by the Textext.ink Inkscape extension
(http://www.iki.fi/pav/software/textext/).

For that, it extracts the TeXmacs (or Latex) description of the equation,
creates on the fly a texmacs file and calls texmacs to edit the equation.
On return, Texmacs generates an svg file containing the upadted image
and description of the equation, which then replaces the old equation.

If no equation exists when the extension is invoked, then a new
equation is created.

We invoke TeXmacs with a command line argument that displays "Done"
(and "Cancel") button in the user toolbar that produces the new
version of the svg-texmacs equation and closes TeXmacs.

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

"""
#------------------------------------------------------------------------------

import sys, os, glob, platform
import inkex, tempfile, subprocess, copy
from lxml import etree

IS_WINDOWS = (platform.system() == "Windows")
#IS_MACOS= sys.platform.startswith('darwin')

if IS_WINDOWS :
    programfiles = os.environ.get('PROGRAMFILES')
    texmacs_path = os.path.join(programfiles, 'TeXmacs','bin', 'texmacs.exe')
    
else : texmacs_path ='texmacs' #texmacs needs to be in the path!

TEXTEXT_NS = u"http://www.iki.fi/pav/software/textext/"
TEXMACS_NS = u"https://www.texmacs.org/"
SVG_NS = u"http://www.w3.org/2000/svg"

NSS = {
    u'textext': TEXTEXT_NS,
    u'texmacs': TEXMACS_NS,
    u'svg': SVG_NS
}

tm_file="<TeXmacs|1.0.7.15>\n\n<style|generic>\n\n<\\body>\n %s \n\n</body>\n\n<\\initial>\n %s \n\n</initial>"
tm_dummy_equation="<\equation*>\n    1+1\n  </equation*>\n"
tm_no_equation="\\;\n"
tm_scheme_cmd_line_args =  "(begin (show-icon-bar 3 #t) (menu-bind texmacs-extra-icons "\
		"((balloon \"Done\" \"update equation in Inkscape\") (((select-all)(export-selection-as-graphics "\
		"(url-glue (current-buffer) \".svg\"))(quit-TeXmacs))))((balloon \"Cancel\" "\
		"\"abandon modifying equation\")(quit-TeXmacs)))%s) "
tm_extra_latex_cmd_line_args=  "(delayed (:idle 000)(insert (latex->texmacs (parse-latex \"\\\\[ %s \\\\]\"))))"
tm_no_style=""

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
        """Perform the effect: create/modify embedded equation"""
        
        # Find equation and how to modify it
        old_node, latex_option_cmd, tm_equation, tm_style = self.get_old()
        
        # build full scheme command line command        
        scheme_cmd = tm_scheme_cmd_line_args % latex_option_cmd

        # call texmacs for editing
        self.call_texmacs(scheme_cmd, tm_equation, tm_style)

        svg_name = self.tmp_name + ".svg" #if successful texmacs creates that svg file
        if os.path.isfile(svg_name):
           f = open(svg_name, 'r')
           tree = etree.parse(f)
           f.close()

           root = tree.getroot()
           new_node = root.find('{%s}g' % SVG_NS)

           # -- Replace
           self.replace_node(old_node, new_node)

        #finish : cleanup
        self.remove_temp_files()

    
    def get_old(self):
        """
        Dig out equation to be modified (texmacs or textext)
        Returns: (old_svg_node, latex_option_command (for the textext case), tm_equation_to_be_edited)
        """
        for i in self.options.ids: 
        # gets list of ids in the call of the extension.
            node = self.selected[i] 
            # selects each member of selection in turn ; 
            #only the first one which can be processed will be
            if node.tag != '{%s}g' % SVG_NS: continue
            
            elif '{%s}texmacscode'%TEXMACS_NS in node.attrib: # that group contains texmacs data
            	tm_equation = node.attrib.get('{%s}texmacscode' % TEXMACS_NS, '').decode('string-escape')
            	if '{%s}texmacstyle'%TEXMACS_NS in node.attrib: #further contains styling info
            	    tm_style = node.attrib.get('{%s}texmacstyle' % TEXMACS_NS, '').decode('string-escape')
            	else:
                    tm_style =''
            	return (node, '', tm_equation, tm_style)

            elif '{%s}text'%TEXTEXT_NS in node.attrib:  #implements Textext conversion to TeXmacs
            	latex_code = node.attrib.get('{%s}text' % TEXTEXT_NS, '')
                return (node, tm_extra_latex_cmd_line_args % latex_code, tm_no_equation, tm_no_style)

		# if we arrive here no editable equation was in
        # selection (including no selection): launch TeXmacs with dummy equation.
        return (None, '', tm_dummy_equation, tm_no_style)

    def call_texmacs(self, scheme_cmd, equ, styl):
			f_tmp = open(self.tmp_name, 'w') # create a temporaty tm file that texmacs will edit
			try:
				f_tmp.write(tm_file %( equ, styl)) #insert equation to be edited in file (blank in textext case)
			finally:
				f_tmp.close()
			cmd = [texmacs_path,"-x",scheme_cmd , self.tmp_name]
			try:
				p = subprocess.Popen(cmd, 
			                     stdout=subprocess.PIPE,
			                     stderr=subprocess.PIPE)
				out, err = p.communicate()
			except OSError, e:
			    raise RuntimeError("Command %s failed: %s" % (' '.join(cmd), e))
        
			if p.returncode != 0 :
			    raise RuntimeError("Command %s failed (code %d): %s"
                               % (' '.join(cmd), p.returncode, out + err))

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
