<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Automatic content generation>

  The <tmdtd|std-automatic> <abbr|d.t.d.> specifies for the automatic
  generation of auxiliary content like tables of contents and bibliographies,
  as well as for the presentation of such auxiliary content. The following
  tags are used for bibliographies:

  <\explain|<explain-macro|cite|ref-1|<with|mode|math|\<cdots\>>|ref-n>>
    Each argument <src-arg|ref-i> is a citation corresponding to an item in a
    BiB-<TeX> file. The citations are displayed in the same way as they are
    referenced in the bibliography and they also provide hyperlinks to the
    correspoding references. The citations are displayed as question marks if
    you did not generate the bibliography.
  </explain>

  <\explain|<explain-macro|nocite|ref-1|<with|mode|math|\<cdots\>>|ref-n>>
    Similar as <markup|cite>, but the citations are not displayed in the main
    text.
  </explain>

  <\explain|<explain-macro|cite-detail|ref|info>>
    A bibliographic reference <src-arg|ref> like above, but with some
    additional information <src-arg|info>, like a chapter or a page number.
  </explain>

  <\explain|<explain-macro|bibitem*|content>>
    Macro which specifies how to display an item in the bibliography. The
    <src-arg|content> may be a single reference, like ``TM98'', or a list of
    references, like ``Euler1, Gauss2''.
  </explain>

  The following tags are used for compiling tables of contents:

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

  <\explain|<explain-macro|toc-dots>>
    The separation between an entry in the table of contents and the
    corresponding page number. By default, we use horizontal dots.
  </explain>

  The following tags are used for indices:

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

  The following tags are used for glossaries:

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