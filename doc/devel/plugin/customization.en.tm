<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Further customization of the interface>

  Having written a working interface between your system and <apply|TeXmacs>,
  you may want to improve it further. Below, we will discuss a few directions
  for possible improvement.

  First of all, you may want to customize the keyboard behavior inside a
  <verbatim|myplugin>-session and add appropriate menus. The mechanisms for
  doing that are described in the chapter about the <name|Guile/Scheme>
  extension language and you may add such support to the file
  <verbatim|init-myplugin.scm>. We again recommend you to take a look at the
  plugins which are shipped with <TeXmacs> inside the directory
  <verbatim|$TEXMACS_HOME_PATH/plugins>.

  Certain output from your system might require a special markup. For
  instance, assume that you want to associate an invisible type to each
  subexpression in the output. Then you may create a macro
  <verbatim|exprtype> with two arguments in <verbatim|myplugin.ts> and send
  <apply|LaTeX> expressions like <verbatim|\\exprtype{1}{Integer}> to
  <apply|TeXmacs> during the output.

  In the case when you connected your system to <apply|TeXmacs> using pipes,
  you may directly execute <apply|TeXmacs> commands during the output from
  your system by incorporating pieces of code of the form:

  <\verbatim>
    \ \ \ \ [DATA_BEGIN]command:scheme-program[DATA_END]
  </verbatim>

  in your output. Inversily, when the cursor is inside a session of your
  system, you may use the <name|Scheme> command:

  <\verbatim>
    \ \ \ \ (extern-exec plugin-command)
  </verbatim>

  in order to execute a command of your system.

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
  </collection>
</references>
