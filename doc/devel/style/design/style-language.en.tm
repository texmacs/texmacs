<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|The style-sheet language>

  In the section about <hyper-link|writing a simple style
  package|style-example.en.tm> we already gave you a first impression about
  the style-sheet language of <TeXmacs>. In this section, we will give a more
  complete survey of the available features. For more detailed descriptions,
  we refer to the chapter about the <hyper-link|<TeXmacs>
  primitives|../../format/primitives/primitives.en.tm>.

  Most style-sheet primitives can be obtained from the <menu|Source> menu
  when you are in source mode. You may also obtain them from the
  <menu|Insert|Macro> and <menu|Insert|Executable> menus when editing usual
  text. Alternatively, you may use the <key|A-> and <key|M-e > prefixes in
  source mode and the <key|M-i > and <key|M-e > prefixes otherwise.
  Furthermore, we recall that the hybrid <key|\\>-key may be used for
  creating macro-applications or arguments, depending on the context.
  Finally, the <key|A-<key-right>> and <key|A-<key-left>> keys are used for
  inserting arguments.

  <\traverse>
    <branch|Assignments|style-assign.en.tm>

    <branch|Macro expansion|style-macro.en.tm>

    <branch|Formatting primitives|style-format.en.tm>

    <branch|Evaluation control|style-eval.en.tm>

    <branch|Flow control|style-flow.en.tm>

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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>