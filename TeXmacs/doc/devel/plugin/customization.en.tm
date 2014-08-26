<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Further customization of the interface>

  Having written a working interface between your system and <TeXmacs>, you
  may want to improve it further. Below we will discuss a few directions for
  possible improvement.

  First of all, you may want to customize the keyboard behavior inside a
  <verbatim|myplugin>-session and add appropriate menus. The procedure for
  doing that is described in the chapter about the <name|Guile/Scheme>
  extension language and you may add such support to the file
  <verbatim|init-myplugin.scm>. We again recommend you to take a look at the
  plugins which are shipped with <TeXmacs> inside the directory
  <verbatim|$TEXMACS_HOME_PATH/plugins>.

  Certain output from your system might require special markup. For instance,
  assume that you want to associate an invisible type to each subexpression
  in the output. Then you may create a macro <verbatim|exprtype> with two
  arguments in <verbatim|myplugin.ts> and send <LaTeX> expressions like
  <verbatim|\\exprtype{1}{Integer}> to <TeXmacs> during the output.

  In case you connected your system to <TeXmacs> using pipes, you may
  directly execute <TeXmacs> commands during the output from your system by
  incorporating pieces of code of the form:

  <\indent>
    <\verbatim>
      [DATA_BEGIN]command:scheme-program[DATA_END]
    </verbatim>
  </indent>

  in your output. Inversely, when the cursor is inside a session of your
  system, you may use the <name|Scheme> command: <todo|This is no longer true
  (svn r8750).>

  <\indent>
    <scm|(extern-exec plugin-command)>
  </indent>

  in order to execute a command of your system.

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