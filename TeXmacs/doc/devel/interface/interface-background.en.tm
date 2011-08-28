<TeXmacs|1.0.4.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Background evaluations>

  Until now, we have always considered interfaces between <TeXmacs> and
  applications which are intended to be used interactively in shell sessions.
  But there also exists a <value|scheme> command

  <\scheme-fragment>
    (plugin-eval <em|plugin> <em|session> <em|expression>)
  </scheme-fragment>

  for evaluating an expression using the application. Here
  <verbatim|<em|plugin>> is the name of the plug-in, <verbatim|<em|session>>
  the name of the session and <verbatim|<em|expression>> a <value|scheme>
  expression which represents a <TeXmacs> tree.

  <paragraph*|The <verbatim|substitute> plug-in>

  Background evaluations may for instance be used in order to provide a
  feature which allows the user to select an expression and replace it by its
  evaluation. For instance, the <verbatim|substitute> plug-in converts
  mathematical <LaTeX> expressions into <TeXmacs>, and it provides the
  <shortcut|(substitute-substitute)> keyboard shortcut for replacing a selected text by its
  conversion. The plug-in consists of the following files

  <\verbatim>
    \ \ \ \ <example-plugin-link|substitute/Makefile>

    \ \ \ \ <example-plugin-link|substitute/progs/init-substitute.scm>

    \ \ \ \ <example-plugin-link|substitute/src/substitute.cpp>
  </verbatim>

  The main evaluation loop of <verbatim|substitute.cpp> simply consists of

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN;

    cout \<less\>\<less\> "latex:$" \<less\>\<less\> buffer \<less\>\<less\>
    "$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Moreover, the configuration file <verbatim|init-substitute.scm> contains
  the following code for replacing a selected region by its evaluation

  <\scheme-fragment>
    (define (substitute-substitute)

    \ \ (import-from (texmacs plugin plugin-cmd))

    \ \ (if (selection-active-any?)

    \ \ \ \ \ \ (let* ((t (tree-\<gtr\>stree (the-selection)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ (u (plugin-eval "substitute" "default" t)))

    \ \ \ \ \ \ \ \ (clipboard-cut "primary")

    \ \ \ \ \ \ \ \ (insert (stree-\<gtr\>tree u)))))
  </scheme-fragment>

  as well as the keyboard shortcut for <shortcut|(substitute-substitute)>:

  <\scheme-fragment>
    (kbd-map

    \ \ ("C-F12" (substitute-substitute)))
  </scheme-fragment>

  Notice that these routines should really be defined in a separate module
  for larger plug-ins.

  <paragraph*|The <verbatim|secure> plug-in>

  Another example of using an interface in the background is the
  <verbatim|secure> plug-in which consists of the files

  <\verbatim>
    \ \ \ \ <example-plugin-link|secure/Makefile>

    \ \ \ \ <example-plugin-link|secure/packages/secure.ts>

    \ \ \ \ <example-plugin-link|secure/progs/init-secure.scm>

    \ \ \ \ <example-plugin-link|secure/progs/secure-secure.scm>

    \ \ \ \ <example-plugin-link|secure/src/secure.cpp>
  </verbatim>

  Just as <verbatim|substitute.cpp> above, the main program
  <verbatim|secure.cpp> just converts mathematical <LaTeX> expressions to
  <TeXmacs>. The <verbatim|secure-secure.scm> module contains the <em|secure>
  <value|scheme> routine <verbatim|latexer>:

  <\scheme-fragment>
    (tm-define (latexer s)

    \ \ (:type (tree -\<gtr\> object))

    \ \ (:synopsis "convert LaTeX string to TeXmacs tree using plugin")

    \ \ (:secure #t)

    \ \ (plugin-eval "secure" "default" (tree-\<gtr\>string s)))
  </scheme-fragment>

  It is important to define <verbatim|latexer> as being secure, so that it
  can be used in order to define additional markup using the <markup|extern>
  primitive. This is done in the style file <verbatim|secure.ts>:

  <\tm-fragment>
    <\inactive*>
      See a LaTeX math command as a TeXmacs expression via plug-in

      <assign|latexer|<macro|x|<extern|latexer|<arg|x>>>>
    </inactive*>
  </tm-fragment>

  After compilation, installation, relaunching <TeXmacs> and selecting
  <menu|Document|Use package|secure>, you will now be able to use
  <markup|latexer> as a new primitive. The primitive takes a mathematical
  <LaTeX> expression as its argument and displays its <TeXmacs> conversion.

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
  </collection>
</initial>