<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Example of a plugin with <name|C++> code>

  <paragraph|The <verbatim|minimal> plugin>

  Consider the example of the <verbatim|minimal> plugin in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  It consists of the following files:

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|minimal/Makefile>

    \ \ \ \ <expand|example-plugin-link|minimal/progs/init-minimal.scm>

    \ \ \ \ <expand|example-plugin-link|minimal/src/minimal.cpp>
  </verbatim>

  In order to try the plugin, you first have to recursively copy the
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

  When relaunching <TeXmacs>, the plugin should now be automatically
  recognized.

  <paragraph|How it works>

  The <verbatim|minimal> plugin demonstrates a minimal interface between
  <TeXmacs> and an extern program; the program <verbatim|minimal.cpp> is
  <apply|hyper-link|explained|../interface/interface-pipes.en.tm> in more
  detail in the chapter about writing interfaces. The initialization file
  <verbatim|init-minimal.scm> essentially contains the following code:

  <\expand|scheme-fragment>
    (plugin-configure minimal

    \ \ (:require (url-exists-in-path? "minimal.bin"))

    \ \ (:launch "minimal.bin")

    \ \ (:session "Minimal"))
  </expand>

  The <expand|scheme-code|:require> option checks whether
  <verbatim|minimal.bin> indeed exists in the path (so this will fail if you
  forgot to run the <verbatim|Makefile>). The <expand|scheme-code|:launch>
  option specifies how to launch the extern program. The <verbatim|:session>
  option indicates that it will be possible to create sessions for the
  <verbatim|minimal> plugin using <apply|menu|Text|Session|Minimal>.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Minimal>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
