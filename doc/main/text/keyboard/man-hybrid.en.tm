<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Hybrid commands and <LaTeX> simulation>

  <apply|TeXmacs> allows you to enter <apply|LaTeX> commands directly from
  the keyboard as follows. You first hit the <key|\\>-key in order to enter
  the hybrid <apply|LaTeX>/<apply|TeXmacs> command mode. Next you type the
  command you wish to execute. As soon as you finished typing your command,
  the left footer displays something like

  <\verbatim>
    \ \ \ \ \<less\>return\<gtr\>: action to be undertaken
  </verbatim>

  When you hit the <key|<expand|key-return>>-key at this stage, your command
  will be executed. For instance, in math-mode, you may create a fraction by
  typing <key|\\ f r a c <expand|key-return>>.

  If the command you have typed is not a (recognized) <apply|LaTeX> command,
  then we first look whether the command is an existing <apply|TeXmacs>
  macro, function or environment (provided by the style file). If so, the
  corresponding macro expansion, function application or environment
  application is created (with the right number of arguments). Otherwise, it
  is assumed that your command corresponds to an environment variable and we
  ask for its value. The <key|\\>-key is always equivalent to one of the
  commands <expand|kbd-ia|l>, <expand|kbd-ia|e>, <expand|kbd-ia|a>,
  <expand|kbd-ia|#> or <expand|kbd-ia|v>.

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
