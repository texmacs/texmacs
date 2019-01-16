<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Dynamic libraries>

  Instead of connecting your system to <TeXmacs> using a pipe, it is also
  possible to connect it as a dynamically linked library. Although
  communication through pipes is usually easier to implement, more robust and
  compatible with gradual output, the second option is faster.

  In order to dynamically link your application to <TeXmacs>, you should
  follow the <TeXmacs> communication protocol, which is specified in the
  following header file:

  <\verbatim>
    \ \ \ \ <simple-link|$TEXMACS_PATH/include/TeXmacs.h>
  </verbatim>

  In this file it is specified that your application should export a data
  structure

  <\cpp-code>
    typedef struct package_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_package;

    \ \ char* (*install) (TeXmacs_exports_1* TeXmacs,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ char* options, char** errors);

    \ \ char* (*evaluate) (char* what, char* session, char** errors);

    } package_exports_1;
  </cpp-code>

  which contains an installation routine for your application, as well as an
  evaluation routine for further input (for more information, see the header
  file). <TeXmacs> will on its turn export a structure

  <\cpp-code>
    typedef struct TeXmacs_exports_1 {

    \ \ char* version_protocol; /* "TeXmacs communication protocol 1" */

    \ \ char* version_TeXmacs;

    } TeXmacs_exports_1;
  </cpp-code>

  It is assumed that each application takes care of its own memory
  management. Hence, strings created by <TeXmacs> will be destroyed by
  <TeXmacs> and strings created by the application need to be destroyed by
  the application.

  The string <verbatim|version_protocol> should contain <verbatim|"TeXmacs
  communication protocol 1"> and the string <verbatim|version_package> the
  version of your package. The routine <verbatim|install> will be called once
  by <TeXmacs> in order to initialize your system with options
  <verbatim|options>. It communicates the routines exported by <TeXmacs> to
  your system in the form of a pointer to a structure of type
  <cpp|TeXmacs_exports_1>. The routine should return a status message like

  <\verbatim>
    \ \ \ \ "yourcas-version successfully linked to TeXmacs"
  </verbatim>

  If installation failed, then you should return <verbatim|NULL> and
  <verbatim|*errors> should contain an error message.

  The routine <verbatim|evaluate> is used to evaluate the expression
  <verbatim|what> inside a <TeXmacs>-session with name <verbatim|session>. It
  should return the evaluation of <verbatim|what> or <verbatim|NULL> if an
  error occurred. <verbatim|*errors> either contains one or more warning
  messages or an error message, if the evaluation failed. The formats being
  used obey the same rules as in the case of communication by pipes.

  Finally, the configuration file of your plug-in should contain something as
  follows:

  <\scm-code>
    (plugin-configure <em|myplugin>

    \ \ (:require (url-exists? (url "$LD_LIBRARY_PATH"
    "lib<em|myplugin>.so")))

    \ \ (:link "lib<em|myplugin>.so" "<em|myplugin>_exports" "")

    \ \ <em|further-configuration>)
  </scm-code>

  Here <verbatim|<em|myplugin>_exports> is a pointer to a structure of the
  type <cpp|package_exports_1>.

  <\remark>
    It is possible that the communication protocol changes in the future. In
    that case, the data structures <verbatim|TeXmacs_exports_1> and
    <verbatim|package_exports_1> will be replaced by data structures
    <verbatim|TeXmacs_exports_n> and <verbatim|package_exports_n>, where
    <verbatim|n> is the version of the protocol. These structures will always
    have the abstract data structures <verbatim|TeXmacs_exports> and
    <verbatim|package_exports> in common, with information about the versions
    of the protocol, <TeXmacs> and your package.
  </remark>

  <paragraph*|The <verbatim|dynlink> plug-in>

  The <verbatim|dynlink> plug-in gives an example of how to write dynamically
  linked libraries. It consists of the following files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|dynlink/Makefile>

    \ \ \ \ <example-plugin-link|dynlink/progs/init-dynlink.scm>

    \ \ \ \ <example-plugin-link|dynlink/src/dynlink.cpp>
  </verbatim>

  The <verbatim|Makefile> contains

  <\quotation>
    <\framed-fragment>
      <\with|par-par-sep|0fn>
        <\verbatim>
          tmsrc = /home/vdhoeven/texmacs/src/TeXmacs

          CXX = g++

          LD \ = g++

          \;

          lib/libtmdynlink.so: src/dynlink.cpp

          \ \ \ \ \ \ \ \ $(CXX) -I$(tmsrc)/include -c src/dynlink.cpp -o
          src/dynlink.o

          \ \ \ \ \ \ \ \ $(LD) -shared -o lib/libtmdynlink.so src/dynlink.o
        </verbatim>
      </with>
    </framed-fragment>
  </quotation>

  so that running it will create a dynamic library
  <verbatim|dynlink/lib/libdynlink.so> from <verbatim|dynlink.cpp>. The
  <verbatim|tmsrc> variable should contain <verbatim|$TEXMACS_PATH>, so as to
  find the include file <verbatim|TeXmacs.h>. The configuration file
  <verbatim|init-dynlink.scm> simply contains

  <\scm-code>
    (plugin-configure dynlink

    \ \ (:require (url-exists? (url "$LD_LIBRARY_PATH"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "libtmdynlink.so")))

    \ \ (:link "libtmdynlink.so" "dynlink_exports" "")

    \ \ (:session "Dynlink"))
  </scm-code>

  As to the <c++> file <verbatim|dynlink.cpp>, it contains a string

  <\cpp-code>
    static char* output= NULL;
  </cpp-code>

  with the last output, the initialization routine

  <\cpp-code>
    char*

    dynlink_install (TeXmacs_exports_1* TM, char* opts, char** errs) {

    \ \ output= (char*) malloc (50);

    \ \ strcpy (output, "\\2verbatim:Started dynamic link\\5");

    \ \ return output;

    }
  </cpp-code>

  the evaluation routine

  <\cpp-code>
    char*

    dynlink_eval (char* what, char* session, char** errors) {

    \ \ free (output);

    \ \ output= (char*) malloc (50 + strlen (what));

    \ \ strcpy (output, "\\2verbatim:You typed ");

    \ \ strcat (output, what);

    \ \ strcat (output, "\\5");

    \ \ return output;

    }
  </cpp-code>

  and the data structure with the public exports:

  <\cpp-code>
    package_exports_1 dynlink_exports= {

    \ \ "TeXmacs communication protocol 1",

    \ \ "Dynlink 1",

    \ \ dynlink_install,

    \ \ dynlink_eval

    };
  </cpp-code>

  Notice that the application takes care of the memory allocation and
  deallocation of <cpp|output>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>