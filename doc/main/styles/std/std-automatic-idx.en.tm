<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Indexes>

  The following macros may be used in the main text for inserting entries
  into the index.

  <\explain|<explain-macro|index|primary>>
    Insert <src-arg|primary> as a primary entry in the index.
  </explain>

  <\explain|<explain-macro|subindex|primary|secondary>>
    Insert <src-arg|secondary> in the index as a subentry of
    <src-arg|primary>.
  </explain>

  <\explain|<explain-macro|subsubindex|primary|secondary|ternary>>
    Similar to <markup|subindex> but for subsubentries <src-arg|ternary>.
  </explain>

  <\explain|<explain-macro|index-complex|key|how|range|entry>>
    Insert complex entries into the index. This feature is documented in
    detail in the section about <hyper-link|index
    generation|../../links/man-index.en.tm>.
  </explain>

  <\explain|<explain-macro|index-line|key|entry>>
    Adds <src-arg|entry> to the index, by sorting it according to
    <src-arg|key>.
  </explain>

  The following macros may be redefined if you want to customize the
  rendering of the index:

  <\explain|<explain-macro|index-1|entry|where>>
    Macro for rendering a principal <src-arg|entry> in the index on page(s)
    <src-arg|where>.
  </explain>

  <\explain|<explain-macro|index-1*|entry|where>>
    Macro for rendering a principal <src-arg|entry> in the index on page(s)
    <src-arg|where>.
  </explain>

  <\explain|<explain-macro|index-n|entry-1|<with|mode|math|\<cdots\>>|entry-n|where>>
    Macro used for rendering subentries in the index. Here <with|mode|math|n>
    is a number between <with|mode|math|1> and <with|mode|math|5>.
  </explain>

  <\explain|<explain-macro|index-n*|entry-1|<with|mode|math|\<cdots\>>|entry-n>>
    Similar to <markup|index-<with|mode|math|n>>, but without the page
    number.
  </explain>

  <\explain|<explain-macro|index-dots>>
    Macro for producing the dots between an index entry and the corresponding
    page number(s).
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