<TeXmacs|1.0.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating your own keyboard shortcuts>

  Keymaps are specified using the command\ 

  <\verbatim>
    \ \ \ \ (kbd-map . <em|keymaps>)
  </verbatim>

  Optionally, you may specify conditions which must be satisfied for the
  keymap to be valid using the <verbatim|:mode> option. For instance, the
  command

  <\verbatim>
    \ \ \ \ (kbd-map (:mode in-math?) . <em|keymaps>)
  </verbatim>

  specifies a list of keyboard shortcuts which will only be valid in
  math-mode. Each item in <verbatim|<em|keymaps>> is of one of the following
  forms:

  <\verbatim>
    \ \ \ \ (<em|key-combination> <em|action_1> ... <em|action_n>)<next-line>
    \ \ \ (<em|key-combination> <em|result>)<next-line>
    \ \ \ (<em|key-combination> <em|result> <em|help-message>)
  </verbatim>

  In the first case, the <verbatim|<em|action_i>> are <scheme> commands
  associated to the string <verbatim|<em|key-combination>>. In the second and
  third case, <verbatim|<em|result>> is a string which is to be inserted in
  the text when the <verbatim|<em|key-combination>> has been completed. An
  optional <verbatim|<em|help-message>> may be displayed when the
  <verbatim|<em|key-combination>> is finished.

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