<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Example of a plug-in with <name|C++> code>

  <paragraph*|The <verbatim|minimal> plug-in>

  Consider the example of the <verbatim|minimal> plug-in in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  It consists of the following files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|minimal/Makefile>

    \ \ \ \ <example-plugin-link|minimal/progs/init-minimal.scm>

    \ \ \ \ <example-plugin-link|minimal/src/minimal.cpp>
  </verbatim>

  In order to try the plug-in, you first have to recursively copy the
  directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/minimal
  </verbatim>

  to <verbatim|$TEXMACS_PATH/progs> or <verbatim|$TEXMACS_HOME_PATH/progs>.
  Next, running the <verbatim|Makefile> using

  <\verbatim>
    \ \ \ \ make
  </verbatim>

  will compile the program <verbatim|minimal.cpp> and create a binary

  <\verbatim>
    \ \ \ \ minimal/bin/minimal.bin
  </verbatim>

  When relaunching <TeXmacs>, the plug-in should now be automatically
  recognized.

  <paragraph*|How it works>

  The <verbatim|minimal> plug-in demonstrates a minimal interface between
  <TeXmacs> and an extern program; the program <verbatim|minimal.cpp> is
  <hyper-link|explained|../interface/interface-pipes.en.tm> in more detail in
  the chapter about writing interfaces. The initialization file
  <verbatim|init-minimal.scm> essentially contains the following code:

  <\scheme-fragment>
    (plugin-configure minimal

    \ \ (:require (url-exists-in-path? "minimal.bin"))

    \ \ (:launch "minimal.bin")

    \ \ (:session "Minimal"))
  </scheme-fragment>

  The <scheme-code|:require> option checks whether <verbatim|minimal.bin>
  indeed exists in the path (so this will fail if you forgot to run the
  <verbatim|Makefile>). The <scheme-code|:launch> option specifies how to
  launch the extern program. The <verbatim|:session> option indicates that it
  will be possible to create sessions for the <verbatim|minimal> plug-in
  using <menu|Insert|Session|Minimal>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>