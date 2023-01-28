#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
*******************************************************************************
* Texmacs extension for Inkscape
* COPYRIGHT  : (C) 2012-2022 Philippe JOYEZ
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
this files goes into ~/.config/Inkscape/extensions
(in Windows, it's in $APPDATA/Inkscape/extensions)

This extension allows to (re-)edit mathematical equations in WYSIWYG
way with GNU-TeXmacs. It works with equations originally produced by
versions of TeXmacs <http://www.texmacs.org> dated after july 2012,
but also with those made by the Textext.ink Inkscape extension
(http://www.iki.fi/pav/software/textext/).

For that, it extracts the TeXmacs (or Latex) description of the equation
(which appears just as an ordinary group in inkscape),
creates on the fly a texmacs file and calls texmacs to edit the equation.
On return, Texmacs generates an svg file containing the updated image
and description of the equation, which then replaces the old equation.

If no equation exists when the extension is invoked, then a new
equation is created.

We either connect to TeXmacs through a socket communication, or launch
it with a command line argument that puts it in equation editor
configuration. In either case TeXmacs then displays "Done" and "Cancel"
buttons in the user toolbar.

This extension is meant to be as much as possible 2-ways compatible with
Textext.ink equations. Note however that in order to correctly edit texmacs
equations, textext has to be invoked with a preamble file containing the
texmacs latex macros and appropriate includes (see texmacs_latex.sty coming
with this file).

For a large part, the inspiration, implementation and technical details of this
script originally came from textext.py (http://www.iki.fi/pav/software/textext/)
, an Inkscape extension written by Pauli Virtanen  (pav@iki.fi) that does
the same thing with Latex equations, albeit in an essentially non-WYSIWYG way.
Part of the code was directly copied from textext.py. Thank you Pauli!

"""
#------------------------------------------------------------------------------

import os, glob, platform, time, shutil
import inkex, tempfile, subprocess
#from xml.etree import ElementTree as etree
from lxml import etree

import socket

IS_WINDOWS = (platform.system() == "Windows")
#IS_MACOS= sys.platform.startswith('darwin')

def myfind():
    pf = 'PROGRAMFILES(X86)' if ('PROGRAMFILES(X86)' in os.environ) else 'PROGRAMFILES'
    # this is the default install path for the installer
    texmacs_path = os.path.join(os.environ[pf], 'TeXmacs', 'bin', 'texmacs.exe')
    if os.path.isfile(texmacs_path) : return texmacs_path
    else : #check if TeXmacs dir was added to PATH
        #inkex.utils.debug(os.get_exec_path())
        for dirname in os.get_exec_path():
            candidate = os.path.join(dirname, 'bin', 'texmacs.exe')
            if os.path.isfile(candidate) :
              return candidate
        return ''    

def texmacs_exe_path() :
  ''' Find texmacs path, save config file to user profile if necessary'''
  texmacs_path = ""
  UserPath = inkex.utils.get_user_directory()
  conf_file = UserPath+'/extensions/texmacs/texmacs_path.conf'
  if os.path.isfile(conf_file):
  # try to load from saved config file
    with open(conf_file, 'r') as f:
      texmacs_path = f.read()
      if not(os.path.isfile(texmacs_path)):
        texmacs_path = ""

  if (texmacs_path == "" ) :
      texmacs_path = shutil.which('texmacs') # looking in $PATH
      if texmacs_path == None :
        texmacs_path = ""
    
  if (texmacs_path == "" ) and IS_WINDOWS :
        texmacs_path = myfind()

  #raise inkex.AbortExtension("texmacs path: " +texmacs_path)
  if texmacs_path == "":
     raise inkex.AbortExtension('''Inkscape cannot connect with TeXmacs, sorry.
Please see the submenu Extensions>TeXmacs equation>Help to enter the path of your TeXmacs executable
or have TeXmacs already running with the equation plugin in socket server mode''')
  else :
    return texmacs_path

def string_unescape(s):
    """
etree.parse uses the encoding specified in the xml file (UTF8)
However when the svg is created, texmacs uses Cork encoding and escapes
the characters above 128 (for instance é => \xe9 ) and some special characters (&,<,>...)
so that the texmacs code recorded by texmacs
and read back by this python code in LO are not immediatly consistent.
Here, we take care that these characters are properly translated back to texmacs
(note also the .encode when writing the file content)
    """
    return bytes(s, 'utf8').decode('unicode_escape')

TEXTEXT_NS = u"http://www.iki.fi/pav/software/textext/"
TEXMACS_NS = u"https://www.texmacs.org/"
TEXMACS_OLD_NS = u"http://www.texmacs.org/"
SVG_NS = u"http://www.w3.org/2000/svg"


tm_file="<TeXmacs|1.99.5>\n\n<style|%s>\n\n<\\body>\n %s \n\n</body>\n\n<\\initial>\n %s \n\n</initial>"
tm_dummy_equation="<\equation*>\n    1+1\n  </equation*>\n"
tm_no_equation="\\;\n"
tm_scheme_cmd_line_args =  '(begin (lazy-plugin-force) (equ-edit-cmdline) %s) '
if IS_WINDOWS :
    tm_extra_latex_cmd_line_args=  "(delayed (:idle 000)(insert (latex->texmacs (parse-latex \\\"\\\\[ %s \\\\]\\\"))))"
else :
    tm_extra_latex_cmd_line_args=  '(delayed (:idle 000)(insert (latex->texmacs (parse-latex \"\\\\[ %s \\\\]\"))))'
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
        old_node, latex_code, tm_equation, tm_style ,tm_style2 = self.get_old()

        # build full scheme command line command
        if latex_code != '' :
            scheme_cmd = tm_scheme_cmd_line_args % (tm_extra_latex_cmd_line_args % latex_code)
        else :
            scheme_cmd = tm_scheme_cmd_line_args % ''

        # call texmacs for editing
        self.call_texmacs(scheme_cmd, tm_equation, tm_style, tm_style2, latex_code)

        svg_name = self.tmp_name + ".svg" #if successful texmacs creates that svg file
        if os.path.isfile(svg_name):
           f = open(svg_name, 'r')
           tree = etree.parse(f)
           f.close()
           #inkex.debug("file read  "+svg_name)
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
            root = self.svg.selected[i]
            #inkex.utils.debug("root: " + root.tag )
            # selects each member of selection in turn ;
            #only the first one which can be processed will be
            if ('{%s}texmacscode' % TEXMACS_NS) in root.attrib :
                node = root
            else :
                node = root.find('.//{%s}g[@{%s}texmacscode]' % (SVG_NS, TEXMACS_NS))
            if node is not None:
                tm_equation = string_unescape(node.attrib.get('{%s}texmacscode' % TEXMACS_NS, ''))
                if '{%s}texmacsstyle'%TEXMACS_NS in node.attrib: #contains styling info (fonts , font size...)
                    tm_style = string_unescape(node.attrib.get('{%s}texmacsstyle' % TEXMACS_NS, ''))
                else:
                    tm_style =''
                if '{%s}texmacsstyle2'%TEXMACS_NS in node.attrib: #further contains document style info
                    tm_style2 = string_unescape(node.attrib.get('{%s}texmacsstyle2' % TEXMACS_NS, ''))
                else:
                    tm_style2 ='generic'
                return (node, '', tm_equation, tm_style, tm_style2)
        
            else : 
                if ('{%s}texmacscode' % TEXMACS_OLD_NS) in root.attrib :
                    node = root
                else :
                    node = root.find('.//{%s}g[@{%s}texmacscode]' % (SVG_NS, TEXMACS_OLD_NS)) 
                if node is not None:
                    tm_equation = string_unescape(node.attrib.get('{%s}texmacscode' % TEXMACS_OLD_NS, ''))
                    if '{%s}texmacsstyle'%TEXMACS_OLD_NS in node.attrib: #further contains styling info
                        tm_style = string_unescape(node.attrib.get('{%s}texmacsstyle' % TEXMACS_OLD_NS, ''))
                    else:
                        tm_style =''
                    return (node, '', tm_equation, tm_style, 'generic')
        
                else :
                    if (('{%s}text' % TEXTEXT_NS) in root.attrib):
                        node = root
                    else :
                        node = root.find('.//{%s}g[@{%s}text]' % (SVG_NS, TEXTEXT_NS)) 
                        if node : #implements Textext conversion to TeXmacs
                            latex_code = node.attrib.get('{%s}text' % TEXTEXT_NS, '')
                            return (latex_code, tm_no_equation, tm_no_style, 'generic')
        
        # if we arrive here no editable equation was in
        # selection (including no selection): launch TeXmacs with dummy equation.
        return (None, '', tm_dummy_equation, tm_no_style,'generic')

    def call_texmacs(self, scheme_cmd, equ, styl, styl2 , latex):
        """" handle various ways of calling and communicating with texmacs """
        f_tmp = open(self.tmp_name, 'wb') # create a temporaty tm file that texmacs will edit
        try:
            f_tmp.write((tm_file %( styl2, equ, styl)).encode("iso-8859-1")) #insert style info & equation to be edited in file (blank in textext case)
        finally:
            f_tmp.close()

#try connecting already running texmacs on socket (spares boot-up time)
        size = 1024
        try:
            clientsocket = socket.create_connection(('localhost', 6561), timeout=0.25)
            time.sleep(.1)
            msg='(0 (remote-login "inkscape" "inkscape"))\n'
            clientsocket.sendall(bytes(str(len(bytes(msg,'utf8')))+ '\n'+msg,'utf8'))
            clientsocket.setblocking(1)
            time.sleep(.1)
            msg = clientsocket.recv(size)
        except Exception as e : #any error : can't connect (texmacs not running or server not started), no answer,...
            print(e)
            use_socket = False
            try :
              clientsocket.close()
            except :
              pass
            #inkex.debug("use_socket = False")
        else:
            if msg.find(b"ready") : use_socket = True
#login was accepted; continue with socket connection (assume tm-service remote-equ is properly setup)
            else : 
              use_socket = False #login failed. user not defined?
              clientsocket.close() # avoid a warning
        if use_socket :
            clientsocket.settimeout(None)
            clientsocket.setblocking(1)
            if IS_WINDOWS :
                aux = self.tmp_name.replace('\\','\\\\')
            else :
                aux = self.tmp_name
            msg = '(0 (remote-equ "%s" "%s"))\n' % (aux , latex)
            clientsocket.sendall(bytes(str(len(bytes(msg,'utf8')))+ '\n'+msg,'utf8'))
            time.sleep(.1)
            data = clientsocket.recv(size)
#            inkex.utils.debug("recvd: " + str(len(data)) + " bytes")
#            inkex.utils.debug("recvd:" + data)
            clientsocket.close()
        else :

# socket connection failed : texmacs not in server mode or not started.
#
# Then, use old method : launch it with proper args on the command line
# and communicate through pipes.
#
# In that case, if texmacs has server mode enabled we want it to
# keep running after this script quits, for subsequent connections.
# This is straightforward on Linux/MacOs.
#
# However on Windows this script would hang until texmacs quits, unless
# Texmacs runs as a completly independent process and not a subprocess of this script.
# The next problem is that, on Windows, launching and independent process is
# incompatible with using stdin/stdout pipes; we thus use a named pipe to know 
# when texmacs has finished editing our first equation (sending "done" or "cancel" on stdout)
# Since python shipped with (windows-)inkscape does not have packages for
# handling nicely such named pipes we need to perform low level calls.
#  
# http://code.activestate.com/lists/python-list/446422/
# https://mail.python.org/pipermail/python-list/2005-March/355623.html
            texmacs_path = texmacs_exe_path()
            if IS_WINDOWS :
                import ctypes
                PIPE_ACCESS_DUPLEX = 0x3
                PIPE_TYPE_MESSAGE = 0x4
                PIPE_READMODE_MESSAGE = 0x2
                PIPE_WAIT = 0
                PIPE_NOWAIT = 0x1
                PIPE_UNLIMITED_INSTANCES = 255
                BUFSIZE = 4096
                NMPWAIT_USE_DEFAULT_WAIT = 0
                INVALID_HANDLE_VALUE = -1
                ERROR_PIPE_CONNECTED = 535

                tmPipename = r"\\.\pipe\namedpipe1"

                hPipe = ctypes.windll.kernel32.CreateNamedPipeW(tmPipename,
                                                 PIPE_ACCESS_DUPLEX,
                                                 PIPE_TYPE_MESSAGE |
                                                 PIPE_READMODE_MESSAGE |
                                                 PIPE_WAIT, PIPE_UNLIMITED_INSTANCES,
                                                 BUFSIZE, BUFSIZE, NMPWAIT_USE_DEFAULT_WAIT,
                                                 None
                                                )
                if (hPipe == INVALID_HANDLE_VALUE):
                    inkex.utils.debug("Error in creating Named Pipe")
                    return
                cmd = '"'+texmacs_path+'" -x "'+scheme_cmd+'" "'+self.tmp_name+'" > '+tmPipename
                
                #print (cmd)
                DETACHED_PROCESS = 8
                CREATE_NEW_PROCESS_GROUP = 512 #required for win7
                p = subprocess.Popen(cmd, shell=True, creationflags=CREATE_NEW_PROCESS_GROUP, close_fds=True)
                time.sleep(1)
                fConnected = ctypes.windll.kernel32.ConnectNamedPipe(hPipe, None)
                if ((fConnected == 0) and (ctypes.windll.kernel32.GetLastError() == ERROR_PIPE_CONNECTED)):
                    fConnected = 1
                if (fConnected != 1) :
                    inkex.utils.debug("Could not connect with "+texmacs_path+"\n using named pipe")
                else :
                    ERROR_MORE_DATA = 234
                    BUFSIZE = 512
                    chBuf = ctypes.create_string_buffer(BUFSIZE)
                    cbRead = ctypes.c_ulong(0)
                    while 1 : # repeat loop if ERROR_MORE_DATA
                        fSuccess = ctypes.windll.kernel32.ReadFile(hPipe, chBuf, BUFSIZE, ctypes.byref(cbRead), None)
                        if (fSuccess == 1) :
                            #print ("Number of bytes read:", cbRead.value)
                            #print (chBuf.value)
                            if ((b"done" in chBuf.value) or (b"cancel" in chBuf.value) ):
                                break
                        elif (ctypes.windll.kernel32.GetLastError() != ERROR_MORE_DATA):
                            inkex.utils.debug("error reading from named pipe")
                            break
                      
                    ctypes.windll.kernel32.FlushFileBuffers(hPipe)
                    ctypes.windll.kernel32.DisconnectNamedPipe(hPipe)
                ctypes.windll.kernel32.CloseHandle(hPipe)
                p.returncode = 0 
                del p

            else : # Linux, MacOS: so much simpler!
                cmd = [texmacs_path,"-x",scheme_cmd , self.tmp_name]
                #try:
                p = subprocess.Popen(cmd, 
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT)
                while p.poll() is None:
                        output = p.stdout.readline()
                        if ((b"done" in output) or (b"cancel" in output) ):
                            p.returncode = 0 # Texmacs may continue to run, we want to avoid a warning https://bugs.python.org/issue38890
                            p.stdout.close() # avoid a warning
                            del p
                            break
                #except OSError as e:
                #    raise RuntimeError("Command %s failed: %s" % (' '.join(cmd), e))
                #except :
                #    inkex.utils.debug("launching texmacs failed   ")
                    

    def replace_node(self, old_node, new_node):
        """
        Replace an XML node old_node with new_node
        in self.document.
        """
        if old_node is None:
            new_node.attrib['transform'] = "scale(0.264583333)" 
            # 0.35277779 is 25.4/72 and this scaling is applied when inkscape *imports* an svg (or pdf) file
            # 0.264583333 is (3/4)*25.4/72 (= 25.4/96) and this scaling is applied when inkscape *pastes* our svg from clipboard
            # we apply it here for having consistent sizes when pasting or creating an equation using this extension
            # if we furthermore want the font size to be "correct" we need to set the image output scale to 1.333 in texmacs
            self.svg.get_current_layer().append(new_node)
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
    e.run()
