<TeXmacs|1.0.3.7>

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

  <\explain|<explain-macro|start-page|page-text>>
    This macro specifies the layout of the first page of a new chapter or
    section.
  </explain>

  <\explain|<explain-macro|odd-page-page|page-text>>
    Similar to <markup|start-page>, but for the layout of ordinary odd pages.
  </explain>

  <\explain|<explain-macro|even-page-page|page-text>>
    Similar to <markup|start-page>, but for the layout of ordinary even
    pages.
  </explain>

  The following tags control the logical header-related actions to be
  undertaken, when specifying a title, an author, or when starting a new
  section.

  <\explain|<explain-macro|header-title|title>>
    This macro is called when specifying of the <src-arg|title> of a
    document.
  </explain>

  <\explain|<explain-macro|header-author|author>>
    This macro is called when specifying of the <src-arg|author>(s) of a
    document.
  </explain>

  <\explain|<explain-macro|header-primary|section-title>>
    This macro is called at the start of each new primary section
    (<abbr|e.g.> <markup|chapter> for book style, or <markup|section> for
    article style).
  </explain>

  <\explain|<explain-macro|header-secondary|section-title>>
    This macro is called at the start of each new secondary section
    (<abbr|e.g.> <markup|section> for book style, or <markup|subsection> for
    article style).
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