<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Page breaking primitives>

  The physical lines in a document are broken into pages in a way similar to
  how paragraphs are hyphenated into lines. The page breaker performs
  <def-index|page filling>, it tries to distribute page items evenly so text
  runs to the bottom of every page. It also tries to avoid <def-index|orphans
  and widows>, which are single or pairs of soft lines separated from the
  rest of their paragraph by a page break, but these can be produced when
  there is no better solution.

  <\explain>
    <explain-macro|no-page-break><explain-synopsis|prevent automatic page
    breaking after this line>
  <|explain>
    Prevent the occurrence of an automatic page break after the current line.
    Set an infinite page breaking penalty for the current line, similarly to
    <markup|no-break>.

    Forbidden page breaking points are overridden by ``new page'' and ``page
    break'' primitives.
  </explain>

  <\explain>
    <explain-macro|no-page-break*><explain-synopsis|prevent automatic page
    breaking before this line>
  <|explain>
    Similar to <markup|no-page-break>, but set the page breaking penalty of
    the previous line.
  </explain>

  <\explain>
    <explain-macro|new-page><explain-synopsis|start a new page after this
    line>
  <|explain>
    Cause the next line to appear on a new page, without filling the current
    page. The page breaker will not try to position the current line at the
    bottom of the page.
  </explain>

  <\explain>
    <explain-macro|new-page*><explain-synopsis|start a new page before this
    line>
  <|explain>
    Similar to <markup|new-page>, but start the new page before the current
    line. This directive is appropriate to use in chapter headings.
  </explain>

  <\explain>
    <explain-macro|page-break><explain-synopsis|force a page break after this
    line>
  <|explain>
    Force a page break after the current line. A forced page break is
    different from a new page, the page breaker will try to position the
    current line at the bottom of the page.

    Use only to fine-tune the automatic page breaking. Ideally, this should
    be a hint similar to <markup|line-break>, but this is implemented as a
    directive, use only with extreme caution.
  </explain>

  <\explain>
    <explain-macro|page-break*><explain-synopsis|force a page break before
    this line>
  <|explain>
    Similar to <markup|page-break>, but force a page break before the current
    line.
  </explain>

  When several ``new page'' and ``page break'' directives apply to the same
  point in the document, only the first one is effective. Any
  <markup|new-page> or <markup|page-break> after the first one in a line is
  ignored. Any <markup|new-page> or <markup|page-break> in a line overrides
  any <markup|new-page*> or <markup|page-break*> in the following line. Any
  <markup|new-page*> or <markup|page-break*> after the first one in a line is
  ignored.

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>