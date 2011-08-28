<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Glossaries>

  The following macros may be used in the main text for inserting glossary
  entries.

  <\explain|<explain-macro|glossary|entry>>
    Insert <src-arg|entry> into the glossary.
  </explain>

  <\explain|<explain-macro|glossary-dup|entry>>
    For creating an additional page number for an <src-arg|entry> which was
    already inserted before.
  </explain>

  <\explain|<explain-macro|glossary-explain|entry|explanation>>
    A function for inserting a glossary <src-arg|entry> with its
    <src-arg|explanation>.
  </explain>

  <\explain|<explain-macro|glossary-line|entry>>
    Insert a glossary <src-arg|entry> without a page number.
  </explain>

  The following macros can be redefined if you want to customize the
  rendering of the glossary:

  <\explain|<explain-macro|glossary-1|entry|where>>
    Macro for rendering a glossary entry and its corresponding page
    number(s).
  </explain>

  <\explain|<explain-macro|glossary-2|entry|explanation|where>>
    Macro for rendering a glossary entry, its explanation, and its page
    number.
  </explain>

  <\explain|<explain-macro|glossary-dots>>
    Macro for producing the dots between a glossary entry and the
    corresponding page number(s).
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