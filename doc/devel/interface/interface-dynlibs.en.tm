<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Dynamic libraries>

  Instead of connecting your system to <apply|TeXmacs> using a pipe, it is
  also possible to connect it as a dynamically linked library. Although
  communication through pipes is usually easier to implement, more robust and
  compatible with gradual output, the second option is faster.

  In order to dynamically link your application to <TeXmacs>, you should
  follow the <TeXmacs> communication protocol, which is specified in the
  following header file:

  <\verbatim>
    \ \ \ \ <expand|simple-link|$TEXMACS_PATH/include/TeXmacs.h>
  </verbatim>

  In this file it is specified that your application should export a data
  structure

  <\expand|cpp-fragment>
    typedef struct package_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_package;

    \ \ char* (*install) (TeXmacs_exports_1* TeXmacs,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ char* options, char** errors);

    \ \ char* (*evaluate) (char* what, char* session, char** errors);

    } package_exports_1;
  </expand>

  which contains an installation routine for your application, as well as an
  evaluation routine for further input (for more information, see the header
  file). <TeXmacs> will on its turn export a structure

  <\expand|cpp-fragment>
    typedef struct TeXmacs_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_TeXmacs;

    } TeXmacs_exports_1;
  </expand>

  It is assumed that each application takes care of its own memory
  management. Hence, strings created by <TeXmacs> will be destroyed by
  <TeXmacs> and strings created by the application need to be destroyed by
  the application.

  The string <verbatim|version_protocol> should contain <verbatim|"TeXmacs
  communication protocol 1"> and the string <verbatim|version_package> the
  version of your package. The routine <verbatim|install> will be called once
  by <apply|TeXmacs> in order to initialize your system with options
  <verbatim|options>. It communicates the routines exported by
  <apply|TeXmacs> to your system in the form of a pointer to a structure of
  type <expand|cpp-code|TeXmacs_exports_1>. The routine should return a
  status message like

  <\verbatim>
    \ \ \ \ "yourcas-version successfully linked to TeXmacs"
  </verbatim>

  If installation failed, then you should return <verbatim|NULL> and
  <verbatim|*errors> should contain an error message.

  The routine <verbatim|evaluate> is used to evaluate the expression
  <verbatim|what> inside a <apply|TeXmacs>-session with name
  <verbatim|session>. It should return the evaluation of <verbatim|what> or
  <verbatim|NULL> if an error occurred. <verbatim|*errors> either contains
  one or more warning messages or an error message, if the evaluation failed.
  The formats being used obey the same rules as in the case of communication
  by pipes.

  Finally, the configuration file of your plugin should contain something as
  follows:

  <\expand|scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ (:require (url-exists? (url "$LD_LIBRARY_PATH"
    "lib<em|myplugin>.so")))

    \ \ (:link "lib<em|myplugin>.so" "<em|myplugin>_exports" "")

    \ \ <em|further-configuration>)
  </expand>

  Here <verbatim|<em|myplugin>_exports> is a pointer to a structure of the
  type <expand|cpp-code|package_exports_1>.

  <\remark>
    It is possible that the communication protocol changes in the future. In
    that case, the data structures <verbatim|TeXmacs_exports_1> and
    <verbatim|package_exports_1> will be replaced by data structures
    <verbatim|TeXmacs_exports_n> and <verbatim|package_exports_n>, where
    <verbatim|n> is the version of the protocol. These structures will always
    have the abstract data structures <verbatim|TeXmacs_exports> and
    <verbatim|package_exports> in common, with information about the versions
    of the protocol, <apply|TeXmacs> and your package.
  </remark>

  <paragraph|The <verbatim|dynlink> plugin>

  The <verbatim|dynlink> plugin gives an example of how to write dynamically
  linked libraries. It consists of the following files:

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|dynlink/Makefile>

    \ \ \ \ <expand|example-plugin-link|dynlink/progs/init-dynlink.scm>

    \ \ \ \ <expand|example-plugin-link|dynlink/src/dynlink.cpp>
  </verbatim>

  The <verbatim|Makefile> contains

  <\quotation>
    <\expand|framed-fragment>
      <\with|interparagraph space|0fn>
        <\with|font family|tt>
          tmsrc = /home/vdhoeven/texmacs/src/TeXmacs

          CXX = g++

          LD \ = g++

          \;

          lib/libtmdynlink.so: src/dynlink.cpp

          \ \ \ \ \ \ \ \ $(CXX) -I$(tmsrc)/include -c src/dynlink.cpp -o
          src/dynlink.o

          \ \ \ \ \ \ \ \ $(LD) -shared -o lib/libtmdynlink.so src/dynlink.o
        </with>
      </with>
    </expand>
  </quotation>

  so that running it will create a dynamic library
  <verbatim|dynlink/lib/libdynlink.so> from <verbatim|dynlink.cpp>. The
  <verbatim|tmsrc> variable should contain <verbatim|$TEXMACS_PATH>, so as to
  find the include file <verbatim|TeXmacs.h>. The configuration file
  <verbatim|init-dynlink.scm> simply contains

  <\expand|scheme-fragment>
    (plugin-configure dynlink

    \ \ (:require (url-exists? (url "$LD_LIBRARY_PATH"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "libtmdynlink.so")))

    \ \ (:link "libtmdynlink.so" "dynlink_exports" "")

    \ \ (:session "Dynlink"))
  </expand>

  As to the <value|cpp> file <verbatim|dynlink.cpp>, it contains a string

  <\expand|cpp-fragment>
    static char* output= NULL;
  </expand>

  with the last output, the initialization routine

  <\expand|cpp-fragment>
    char*

    dynlink_install (TeXmacs_exports_1* TM, char* opts, char** errs) {

    \ \ output= (char*) malloc (50);

    \ \ strcpy (output, "\\2verbatim:Started dynamic link\\5");

    \ \ return output;

    }
  </expand>

  the evaluation routine

  <\expand|cpp-fragment>
    char*

    dynlink_eval (char* what, char* session, char** errors) {

    \ \ free (output);

    \ \ output= (char*) malloc (50 + strlen (what));

    \ \ strcpy (output, "\\2verbatim:You typed ");

    \ \ strcat (output, what);

    \ \ strcat (output, "\\5");

    \ \ return output;

    }
  </expand>

  and the data structure with the public exports:

  <\expand|cpp-fragment>
    package_exports_1 dynlink_exports= {

    \ \ "TeXmacs communication protocol 1",

    \ \ "Dynlink 1",

    \ \ dynlink_install,

    \ \ dynlink_eval

    };
  </expand>

  Notice that the application takes care of the memory allocation and
  deallocation of <expand|cpp-code|output>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|1|?>>
    <associate|toc-3|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|dynlink>
      plugin<value|toc-dots><pageref|toc-1>>
    </associate>
  </collection>
</auxiliary>
