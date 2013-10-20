<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Miscellaneous markup>

  Some other potentially useful macros are the following:

  <\explain|<markup|tm-fragment>>
    For indicating some <TeXmacs> document fragment. This macro is especially
    useful for <TeXmacs> source code, as in

    <\tm-fragment>
      <inactive*|<assign|red-text|<macro|body|<with|color|red|<arg|body>>>>>
    </tm-fragment>

    In this example, we used the keyboard shortcut <shortcut|(make-mod-active
    'inactive*)> in order to deactivate the source code inside an active
    outer document.
  </explain>

  <\explain|<markup|descriptive-table>>
    For descriptive tables; such tables can be used to document lists of
    keyboard shortcuts, different types of markup, <abbr|etc.>
  </explain>

  <tmdoc-copyright|1998--2011|Joris van der Hoeven>

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