<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Example of a plugin with <value|scheme> code>

  <paragraph|The <verbatim|world> plugin>

  Consider the <verbatim|world> plugin in the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  This plugin shows how to extend <TeXmacs> with some additional
  <value|scheme> code in the file

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|world/progs/init-world.scm>
  </verbatim>

  In order to test the <verbatim|world> plugin, you should recursively copy
  the directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/world
  </verbatim>

  to <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>.
  When relaunching <TeXmacs>, the plugin should now be automatically
  recognized (a <apply|menu|World> menu should appear in the menu bar).

  <paragraph|How it works>

  The file <verbatim|init-world.scm> essentially contains the following code:

  <\expand|scheme-fragment>
    (define (world-initialize)

    \ \ (menu-extend texmacs-extra-menu

    \ \ \ \ (=\<gtr\> "World"

    \ \ \ \ \ \ \ \ ("Hello world" (insert-string "Hello world")))))

    \;

    (plugin-configure world

    \ \ (:require #t)

    \ \ (:initialize (world-initialize)))
  </expand>

  The configuration option <expand|scheme-code|:require> specifies a
  condition which needs to be satisfied for the plugin to be detected by
  <TeXmacs> (later on, this will for instance allow us to check whether
  certain programs exist on the system). The configuration is aborted if the
  requirement is not fulfilled.

  The option <expand|scheme-code|:initialize> specifies an instruction which
  will be executed during the initialization (modulo the fulfillment of the
  requirement). In our example, we just create a new top level menu
  <apply|menu|World> and a menu item <apply|menu|World|Hello world>, which
  can be used to insert the text ``Hello world''. In general, the
  initialization routine should be very short and rather load a module which
  takes care of the real initialization. Indeed, keeping the
  <verbatim|init-<em|myplugin>.scm> files simple will reduce the startup time
  of <TeXmacs>.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|World>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|World>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|World>|<with|font
      family|<quote|ss>|Hello world>>|<pageref|idx-3>>
    </associate>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|world>
      plugin<value|toc-dots><pageref|toc-1>>

      <with|left margin|<quote|6fn>|font size|<quote|0.84>|How it
      works<value|toc-dots><pageref|toc-2>>
    </associate>
  </collection>
</auxiliary>
