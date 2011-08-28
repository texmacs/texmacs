<TeXmacs|1.0.2.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Hybrid commands and <LaTeX> simulation>

  <TeXmacs> allows you to enter <LaTeX> commands directly from the keyboard
  as follows. You first hit the <key|\\>-key in order to enter the hybrid
  <LaTeX>/<TeXmacs> command mode. Next you type the command you wish to
  execute. As soon as you finished typing your command, the left footer
  displays something like

  <\verbatim>
    \ \ \ \ \<less\>return\<gtr\>: action to be undertaken
  </verbatim>

  When you hit the <shortcut|(kbd-return)>-key at this stage, your command will be
  executed. For instance, in math-mode, you may create a fraction by typing
  <key|\\ f r a c return>.

  If the command you have typed is not a (recognized) <LaTeX> command, then
  we first look whether the command is an existing <TeXmacs> macro, function
  or environment (provided by the style file). If so, the corresponding macro
  expansion, function application or environment application is created (with
  the right number of arguments). Otherwise, it is assumed that your command
  corresponds to an environment variable and we ask for its value. The
  <key|\\>-key is always equivalent to one of the commands <key|inactive l>,
  <key|inactive e>, <key|inactive a>, <key|inactive #> or <key|inactive v>.

  To insert a literal <kbd|\\> (backslash) character, you can use the
  <key|symbol \\> sequence.

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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>