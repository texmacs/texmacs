<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|The style-sheet language>

  In the section about <hlink|writing a simple style
  package|style-example.en.tm> we already gave you a first impression about
  the style-sheet language of <TeXmacs>. In this section, we will give a more
  complete survey of the available features. For more detailed descriptions,
  we refer to the chapter about the <hlink|<TeXmacs>
  primitives|../../format/primitives/primitives.en.tm>.

  The style-sheet primitives can be obtained from the <menu|Source> menu when
  you are in source mode. In all other modes, the <menu|Source> menu becomes
  visible after enabling the <menu|Source macros tool> in the <menu|Tools>
  menu. Alternatively, you may use the <prefix|A-> and <prefix|executable>
  keyboard prefixes in source mode and the<nbsp><prefix|inactive>
  and<nbsp><prefix|executable> prefixes otherwise. Furthermore, we recall
  that the hybrid <key|\\>-key may be used for creating macro-applications or
  arguments, depending on the context. Finally, the
  <shortcut|(structured-insert-right)> and <shortcut|(structured-insert-left)>
  keys are used for inserting arguments.

  <\traverse>
    <branch|Assignments|style-assign.en.tm>

    <branch|Macro expansion|style-macro.en.tm>

    <branch|Formatting primitives|style-format.en.tm>

    <branch|Evaluation control|style-eval.en.tm>

    <branch|Control flow|style-flow.en.tm>

    <branch|Computational markup|style-compute.en.tm>
  </traverse>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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