<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Standard headers>

  The <tmdtd|header> <abbr|d.t.d.> provides tags for customizing the headers
  and footers. The customization is based on the idea that we may specify a
  <em|page text> for every page. This page text can for instance be a running
  title or the name of the current section. The page text may depend on the
  parity of a page and appear in a different way for special pages like
  starts of new chapters. The following tags control the physical layout of
  different types of pages:

  <\description>
    <expand|item*|<markup|start-page>>This tag, with the page text as its
    only argument, specifies the layout of the first page of a new chapter or
    section.

    <expand|item*|<markup|odd-page-text>>Similar to <markup|start-page>, but
    for the layout of ordinary odd pages.

    <expand|item*|<markup|even-page-text>>Similar to <markup|start-page>, but
    for the layout of ordinary even pages.
  </description>

  The following tags control the logical header-related actions to be
  undertaken, when specifying a title, an author, or when starting a new
  section.

  <\description>
    <expand|item*|<markup|header-title>>A tag with a ``title argument'' which
    is used at the specification of the document title.

    <expand|item*|<markup|header-author>>A tag with an ``author argument''
    which is used at the specification of the document author.

    <expand|item*|<markup|header-primary>>A tag with a ``section name
    argument'' which is used at the start of each new primary section
    (<abbr|i.e.> <markup|chapter> for book style, or <markup|section> for
    article style).

    <expand|item*|<markup|header-secondary>>A tag with a ``section name
    argument'' which is used at the start of each new secondary section
    (<abbr|i.e.> <markup|section> for book style, or <markup|subsection> for
    article style).
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
