<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Tables of contents>

  The following macros may be used in the main text for adding entries to the
  table of contents. They are automatically called by most sectional macros,
  but it is sometimes desirable to manually add additional entries.

  <\explain|<explain-macro|toc-main-1|entry>>
    Create a primordial <src-arg|entry> in the table of contents. A
    ``primordial entry'' may for instance be a part of a book.
  </explain>

  <\explain|<explain-macro|toc-main-2|entry>>
    Create a main <src-arg|entry> in the table of contents, like a chapter in
    a book.
  </explain>

  <\explain|<explain-macro|toc-normal-1|entry>>
    Add a normal <src-arg|entry> to the table of contents, like a section.
  </explain>

  <\explain|<explain-macro|toc-normal-2|entry>>
    Add a less important <src-arg|entry> to the table of contents, like a
    subsection.
  </explain>

  <\explain|<explain-macro|toc-normal-3|entry>>
    Add an even less important <src-arg|entry> to the table of contents, like
    a subsubsection.
  </explain>

  <\explain|<explain-macro|toc-small-1|entry>>
    Add a quite unimportant <src-arg|entry> to the table of contents, like a
    paragraph. Since such entries are not very important, some styles may
    simply ignore the <markup|toc-small-1> tag.
  </explain>

  <\explain|<explain-macro|toc-small-2|entry>>
    Similar to <markup|toc-small-1>, but for even less important entries,
    like subparagraphs.
  </explain>

  No macros are provided yet for manually customizing the rendering of tables
  of contents. You may only change the dots between entries and the
  corresponding page number:

  <\explain|<explain-macro|toc-dots>>
    The separation between an entry in the table of contents and the
    corresponding page number. By default, we use horizontal dots.
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