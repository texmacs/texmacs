<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Linking your system as a dynamic library>

  Instead of connecting your system to <apply|TeXmacs> using a pipe, it is
  also possible to connect it as a dynamically linked library. Although
  communication through pipes is usually easier to implement, more robust and
  compatible with gradual output, the second option is faster.

  <section|Connections via dynamically linked libraries>

  Let us now describe the steps you have to go through in order to link your
  system as a dynamic library.

  <\enumerate>
    <item>Modify the architecture of your system in such a way that the main
    part of it can be linked as a shared library; your binary should
    typically become a very small program, which handles verbatim input and
    output, and which is linked with your shared library at runtime.

    <item>Copy the include file <verbatim|$TEXMACS_PATH/include/TeXmacs.h>
    into the include directory of your system's source and write the
    input/output routines as required by the last <TeXmacs> communication
    protocol as explained below.

    <item>Include a line of the form:\ 

    <\verbatim>
      \ \ \ \ (package-declare "myplugin" "libmyplugin.so" "get_name_package"
      "init")
    </verbatim>

    in your file <verbatim|init-myplugin.scm> which has been described in the
    case of communication by pipes. Here <verbatim|libmyplugin.so> is the
    corresponding shared library, <verbatim|get_name_package> the function
    which will be called by <apply|TeXmacs> in order to link your system to
    <apply|TeXmacs>, and <verbatim|init> some initialization string for your
    package.

    <item>Proceed in a similar way as in the case of communication by pipes.
  </enumerate>

  <section|The <TeXmacs> communication protocol>

  The <apply|TeXmacs> communication protocol is used for linking libraries
  dynamically to <apply|TeXmacs>. The file
  <verbatim|$TEXMACS_PATH/include/TeXmacs.h> contains the declarations of all
  data structures and functions used by the protocol. Actually, we foresee a
  succession of different protocols. Each of these protocols have the
  abstract data structures <verbatim|TeXmacs_exports> and
  <verbatim|package_exports> in common, with information about the versions
  of the protocol, <apply|TeXmacs> and your package.

  The <with|mode|math|n>-th concrete version of the communication protocol
  should provide two data structures <verbatim|TeXmacs_exports_n> and
  <verbatim|package_exports_n>. The first structure contains all routines and
  data of <apply|TeXmacs>, which may be necessary for the package. The second
  structure contains all routines and data of your package, which should be
  visible inside <apply|TeXmacs>.

  In order to link your system to <apply|TeXmacs>, you have to implement a
  function:

  <\verbatim>
    \ \ \ \ package_exports* get_my_package (int version);
  </verbatim>

  This function takes the highest <apply|TeXmacs> communication protocol
  supported by your <apply|TeXmacs> system on input. It should return a
  pointer to an instance of a concrete structure
  <verbatim|package_exports_n>, where <verbatim|n> is inferior or equal to
  <verbatim|version>.

  <section|Version 1 of the <TeXmacs> communication protocol>

  In the first version of the <TeXmacs> communication protocol, your package
  should export an instance of the following data structure:\ 

  <\verbatim>
    \ \ \ \ typedef struct package_exports_1 {<format|next line>
    \ \ \ \ \ char* version_protocol; /* "TeXmacs communication protocol 1"
    */<format|next line> \ \ \ \ \ char* version_package;<format|next line>
    \ \ \ \ \ char* (*install) (TeXmacs_exports_1* TM, char* options, char**
    errors);<format|next line> \ \ \ \ \ char* (*evaluate) (char* what, char*
    session, char** errors);<format|next line> \ \ \ \ \ char* (*execute)
    (char* what, char* session, char** errors);<format|next line> \ \ \ }
    package_exports_1;
  </verbatim>

  The string <verbatim|version_protocol> should contain <verbatim|"TeXmacs
  communication protocol 1"> and the string <verbatim|version_package> the
  version of your package.

  The routine <verbatim|install> will be called once by <apply|TeXmacs> in
  order to initialize your system with options <verbatim|options>. It
  communicates the routines exported by <apply|TeXmacs> to your system in the
  form of <verbatim|TM>. The routine should return a status message like:\ 

  <\verbatim>
    \ \ \ \ "yourcas-version successfully linked to TeXmacs"
  </verbatim>

  If installation failed, then you should return <verbatim|NULL> and
  <verbatim|*errors> should contain an error message. Both <verbatim|what>
  and the returned string have a special format, in which it is possible to
  encode arbitrary <apply|TeXmacs> documents. This format will be explained
  in the next section.

  The routine <verbatim|evaluate> is used to evaluate the expression
  <verbatim|what> inside a <apply|TeXmacs>-session with name
  <verbatim|session>. It should return the evaluation of <verbatim|what> or
  <verbatim|NULL> if an error occurred. <verbatim|*errors> either contains
  one or more warning messages or an error message, if the evaluation failed.
  The command:\ 

  <\verbatim>
    \ \ \ \ (package-format "yourcas" "input-format" "output-format")
  </verbatim>

  is used in order to specify the input and output formats for evaluations,
  in a similar way as in the case of pipes.

  The routine <verbatim|execute> has a similar specification as
  <verbatim|evaluate>, except that it is not used for the evaluation of
  expressions inside a <apply|TeXmacs>-session, but rather for other
  communication purposes between <apply|TeXmacs> and your package.

  <\remark>
    All strings returned by the routines <verbatim|install>,
    <verbatim|evaluate> and <verbatim|execute>, as well as all warning and
    error messages should be allocated using <verbatim|malloc>. They will be
    freed by <apply|TeXmacs> using <verbatim|free>.
  </remark>

  The first version of the <apply|TeXmacs> communication protocol also
  requires <apply|TeXmacs> to export an instance of the data structure:

  <\verbatim>
    \ \ \ \ typedef struct TeXmacs_exports_1 {<format|next line>
    \ \ \ \ \ char* version_protocol; /* "TeXmacs communication protocol 1"
    */<format|next line> \ \ \ \ \ char* version_TeXmacs;<format|next line>
    \ \ \ } TeXmacs_exports_1;
  </verbatim>

  The string <verbatim|version_protocol> contains the version
  <verbatim|"TeXmacs communication protocol 1"> of the protocol and
  <verbatim|version_TeXmacs> the current version of <TeXmacs>.

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Connections via dynamically linked
      libraries><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>The TeXmacs communication
      protocol><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Version 1 of the TeXmacs communication
      protocol><value|toc-dots><pageref|toc-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
