<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard headers>

  The <tmdtd|header> <abbr|d.t.d.> provides call-back macros which allow page
  headers and footers to change automatically when specifying the title
  information of the document or when starting a new section.

  <\explain|<explain-macro|header-title|title>>
    This macro is called when specifying the <src-arg|title> of a document.
  </explain>

  <\explain|<explain-macro|header-author|author>>
    This macro is called when specifying the <src-arg|author>(s) of a
    document.
  </explain>

  <\explain|<explain-macro|header-primary|section-title|section-nr|section-type>>
    This macro is called at the start of each new primary section
    (<abbr|e.g.> <markup|chapter> for book style, or <markup|section> for
    article style). The <src-arg|section-type> is a literal text like
    ``Chapter'' or ``Section''.
  </explain>

  <\explain|<explain-macro|header-secondary|section-title|section-nr|section-type>>
    This macro is called at the start of each new secondary section
    (<abbr|e.g.> <markup|section> for book style, or <markup|subsection> for
    article style). The <src-arg|section-type> is a literal text like
    ``Section'' or ``Paragraph''.
  </explain>

  In style files, page headers and footers are usually set by the above
  call-back macros, and not manually. You may directly modify headers and
  footers by setting the <hyper-link|corresponding environment
  variables|../../../devel/format/environment/env-page.en.tm#header-footer-env>
  or using several <hyper-link|helper macros|../std/std-utils-dtd.en.tm#header-footer-helper>
  supplied by <tmpackage|std-utils>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>