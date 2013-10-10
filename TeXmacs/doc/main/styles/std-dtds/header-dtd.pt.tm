<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard headers>

  The <tmdtd|header> <abbr|d.t.d.> provides tags for customizing the headers
  and footers. The customization is based on the idea that we may specify a
  <em|page text> for every page. This page text can for instance be a running
  title or the name of the current section. The page text may depend on the
  parity of a page and appear in a different way for special pages like
  starts of new chapters. The following tags control the physical layout of
  different types of pages:

  <\explain|<markup|start-page>>
    This tag, with the page text as its only argument, specifies the layout
    of the first page of a new chapter or section.
  </explain>

  <\explain|<markup|odd-page-text>>
    Similar to <markup|start-page>, but for the layout of ordinary odd pages.
  </explain>

  <\explain|<markup|even-page-text>>
    Similar to <markup|start-page>, but for the layout of ordinary even
    pages.
  </explain>

  The following tags control the logical header-related actions to be
  undertaken, when specifying a title, an author, or when starting a new
  section.

  <\explain|<markup|header-title>>
    A tag with a ``title argument'' which is used at the specification of the
    document title.
  </explain>

  <\explain|<markup|header-author>>
    A tag with an ``author argument'' which is used at the specification of
    the document author.
  </explain>

  <\explain|<markup|header-primary>>
    A tag with a ``section name argument'' which is used at the start of each
    new primary section (<abbr|i.e.> <markup|chapter> for book style, or
    <markup|section> for article style).
  </explain>

  <\explain|<markup|header-secondary>>
    A tag with a ``section name argument'' which is used at the start of each
    new secondary section (<abbr|i.e.> <markup|section> for book style, or
    <markup|subsection> for article style).
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
  </collection>
</initial>