<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Introduction>

  In this chapter we describe how to interface <TeXmacs> with an extern
  application. Such interfaces should be distributed in the form of
  <apply|hyper-link|plugins|../plugin/plugins.en.tm>. The plugin may either
  contain the extern application, or provide the ``glue'' between <TeXmacs>
  and the application. Usually, interfaces are used interactively in shell
  sessions (see <apply|menu|Text|Session>). But they may also be designed for
  background tasks, such as spell checking or typesetting.

  The communication between <TeXmacs> and the application takes place using a
  customizable input format and the special <em|<TeXmacs> meta-format> for
  output from the plugin. The meta-format enables you to send structured
  output to <TeXmacs>, using any common format like <verbatim|verbatim>,
  <LaTeX>, <name|Postscript>, <name|HTML,> or <TeXmacs> itself. This is
  useful when adding a <TeXmacs> interface to an existing system, since
  <LaTeX> or <name|Postscript> output routines are often already implemented.
  It will then suffice to put the appropriate markers in order to make a
  first interface with <TeXmacs>.

  As soon as basic communication between your application and <TeXmacs> is
  working, you may improve the interface in many ways. Inside shell sessions,
  there is support for prompts, default inputs, tab-completion, mathematical
  and multi-line input, <abbr|etc.> In general, your application may take
  control of <TeXmacs> and modify the user interface (menus, keyboard,
  <abbr|etc.>) or add new <value|scheme> routines to <TeXmacs>. Your
  application may even extend the typesetter.

  In the directory <verbatim|$TEXMACS_PATH/examples/plugins>, you can find
  many examples of simple plugins. In the next sections, we will give a more
  detailed explanation of the interfacing features of <TeXmacs> on the hand
  of these examples. In order to try one of these examples, we recall that
  you just have to copy it to either one of the directories

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/plugins

    \ \ \ \ $TEXMACS_HOME_PATH/plugins
  </verbatim>

  and run the <verbatim|Makefile> (if there is one).

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

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
