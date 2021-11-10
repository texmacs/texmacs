<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating your own keyboard shortcuts>

  Keymaps are specified using the command

  <\scm-fragment>
    (kbd-map . <scm-arg|keymaps>)
  </scm-fragment>

  Optionally, you may specify conditions which must be satisfied for the
  keymap to be valid using the <scm|:mode> option. For instance, the command

  <\scm-fragment>
    (kbd-map (:mode in-math?) . <scm-arg|keymaps>)
  </scm-fragment>

  specifies a list of keyboard shortcuts which will only be valid in
  math-mode. Each item in <scm-arg|keymaps> is of one of the following forms:

  <\scm-fragment>
    (<em|key-combination> <scm-arg|action_1> ... <scm-arg|action_n>)

    (<em|key-combination> <scm-arg|result>)

    (<em|key-combination> <scm-arg|result> <scm-arg|help-message>)
  </scm-fragment>

  In the first case, the <scm-arg|action_i> are <scheme> commands associated
  to the string <scm-arg|key-combination>. In the second and third case,
  <scm-arg|result> is a string which is to be inserted in the text when the
  <scm-arg|key-combination> has been completed. An optional
  <scm-arg|help-message> may be displayed when the <scm-arg|key-combination>
  is finished.

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