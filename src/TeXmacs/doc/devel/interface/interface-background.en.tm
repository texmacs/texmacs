<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Background evaluations>

  Until now, we have always considered interfaces between <TeXmacs> and
  applications which are intended to be used interactively in shell sessions.
  But there also exists a <value|scheme> command

  <\expand|scheme-fragment>
    (plugin-eval <em|plugin> <em|session> <em|expression>)
  </expand>

  for evaluating an expression using the application. Here
  <verbatim|<em|plugin>> is the name of the plugin, <verbatim|<em|session>>
  the name of the session and <verbatim|<em|expression>> a <value|scheme>
  expression which represents a <TeXmacs> tree.

  <paragraph|The <verbatim|substitute> plugin>

  Background evaluations may for instance be used in order to provide a
  feature which allows the user to select an expression and replace it by its
  evaluation. For instance, the <verbatim|substitute> plugin converts
  mathematical <LaTeX> expressions into <TeXmacs>, and it provides the
  <key|C-F12> keyboard shortcut for replacing a selected text by its
  conversion. The plugin consists of the following files

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|substitute/Makefile>

    \ \ \ \ <expand|example-plugin-link|substitute/progs/init-substitute.scm>

    \ \ \ \ <expand|example-plugin-link|substitute/src/substitute.cpp>
  </verbatim>

  The main evaluation loop of <verbatim|substitute.cpp> simply consists of

  <\expand|cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN;

    cout \<less\>\<less\> "latex:$" \<less\>\<less\> buffer \<less\>\<less\>
    "$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  Moreover, the configuration file <verbatim|init-substitute.scm> contains
  the following code for replacing a selected region by its evaluation

  <\expand|scheme-fragment>
    (define (substitute-substitute)

    \ \ (import-from (texmacs plugin plugin-cmd))

    \ \ (if (selection-active-any?)

    \ \ \ \ \ \ (let* ((t (tree-\<gtr\>object (the-selection)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ (u (plugin-eval "substitute" "default" t)))

    \ \ \ \ \ \ \ \ (clipboard-cut "primary")

    \ \ \ \ \ \ \ \ (insert-tree (object-\<gtr\>tree u)))))
  </expand>

  as well as the keyboard shortcut for <key|C-F12>:

  <\expand|scheme-fragment>
    (kbd-map

    \ \ ("C-F12" (substitute-substitute)))
  </expand>

  Notice that these routines should really be defined in a separate module
  for larger plugins.

  <paragraph|The <verbatim|secure> plugin>

  Another example of using an interface in the background is the
  <verbatim|secure> plugin which consists of the files

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|secure/Makefile>

    \ \ \ \ <expand|example-plugin-link|secure/packages/secure.ts>

    \ \ \ \ <expand|example-plugin-link|secure/progs/init-secure.scm>

    \ \ \ \ <expand|example-plugin-link|secure/progs/secure-secure.scm>

    \ \ \ \ <expand|example-plugin-link|secure/src/secure.cpp>
  </verbatim>

  Just as <verbatim|substitute.cpp> above, the main program
  <verbatim|secure.cpp> just converts mathematical <LaTeX> expressions to
  <TeXmacs>. The <verbatim|secure-secure.scm> module contains the <em|secure>
  <value|scheme> routine <verbatim|latexer>:

  <\expand|scheme-fragment>
    (tm-define (latexer s)

    \ \ (:type (string -\<gtr\> object))

    \ \ (:synopsis "convert LaTeX string to TeXmacs tree using plugin")

    \ \ (:secure #t)

    \ \ (plugin-eval "secure" "default" s))
  </expand>

  It is important to define <verbatim|latexer> as being secure, so that it
  can be used in order to define additional markup using the <markup|extern>
  primitive. This is done in the style file <verbatim|secure.ts>:

  <\expand|tm-fragment>
    <\with|preamble|true>
      See a LaTeX math command as a TeXmacs expression via plugin

      <assign|latexer|<func|x|<extern|latexer|<apply|x>>>>
    </with>
  </expand>

  After compilation, installation, relaunching <TeXmacs> and selecting
  <apply|menu|Document|Use package|secure>, you will now be able to use
  <markup|latexer> as a new primitive. The primitive takes a mathematical
  <LaTeX> expression as its argument and displays its <TeXmacs> conversion.

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
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|extern>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Use package>|<with|font
      family|<quote|ss>|secure>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|latexer>>|<pageref|idx-3>>
    </associate>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|substitute>
      plugin<value|toc-dots><pageref|toc-1>>

      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|secure>
      plugin<value|toc-dots><pageref|toc-2>>
    </associate>
  </collection>
</auxiliary>
