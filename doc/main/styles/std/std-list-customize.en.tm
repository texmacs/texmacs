<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Customization of list environments>

  The <tmdtd|std-list> provides the following redefinable macros for
  customizing the rendering of lists and items in lists:

  <\explain|<explain-macro|render-list|body>>
    This block environment is used to render the <src-arg|body> of the list.
    Usually, the macro indents the body and puts some vertical space around
    it.
  </explain>

  <\explain|<explain-macro|aligned-item|item-text>>
    This inline macro is used to render the <src-arg|item-text> in a
    right-aligned way. As a consequence, text after such items will appear in
    a left-aligned way.
  </explain>

  <\explain|<explain-macro|compact-item|item-text>>
    This inline macro is used to render the <src-arg|item-text> in a
    left-aligned way. As a consequence, text after such items may be indented
    by the width of the <src-arg|item-text> (except when the text is rendered
    on a different paragraph).
  </explain>

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