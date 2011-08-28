<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Example of a plug-in with <value|scheme> code>

  <paragraph*|The <verbatim|world> plug-in>

  Consider the <verbatim|world> plug-in in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  This plug-in shows how to extend <TeXmacs> with some additional
  <value|scheme> code in the file

  <\verbatim>
    \ \ \ \ <example-plugin-link|world/progs/init-world.scm>
  </verbatim>

  In order to test the <verbatim|world> plug-in, you should recursively copy
  the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/world
  </verbatim>

  to <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>.
  When relaunching <TeXmacs>, the plug-in should now be automatically
  recognized (a <menu|World> menu should appear in the menu bar).

  <paragraph*|How it works>

  The file <verbatim|init-world.scm> essentially contains the following code:

  <\scheme-fragment>
    (define (world-initialize)

    \ \ (menu-extend texmacs-extra-menu

    \ \ \ \ (=\<gtr\> "World"

    \ \ \ \ \ \ \ \ ("Hello world" (insert-string "Hello world")))))

    \;

    (plugin-configure world

    \ \ (:require #t)

    \ \ (:initialize (world-initialize)))
  </scheme-fragment>

  The configuration option <scheme-code|:require> specifies a condition which
  needs to be satisfied for the plug-in to be detected by <TeXmacs> (later
  on, this will for instance allow us to check whether certain programs exist
  on the system). The configuration is aborted if the requirement is not
  fulfilled.

  The option <scheme-code|:initialize> specifies an instruction which will be
  executed during the initialization (modulo the fulfillment of the
  requirement). In our example, we just create a new top level menu
  <menu|World> and a menu item <menu|World|Hello world>, which can be used to
  insert the text ``Hello world''. In general, the initialization routine
  should be very short and rather load a module which takes care of the real
  initialization. Indeed, keeping the <verbatim|init-<em|myplugin>.scm> files
  simple will reduce the startup time of <TeXmacs>.

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