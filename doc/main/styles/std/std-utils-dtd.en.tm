<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Utilities for writing style files>

  The <tmdtd|std-utils> package provides several macros which may be useful
  when writing style files. First of all, the following macros may be used
  for rendering purposes:

  <\explain>
    <explain-macro|hflush>

    <explain-macro|left-flush>

    <explain-macro|right-flush>
  <|explain>
    Low level tags for flushing to the right in the definition of
    environments. One usually should use <markup|wide-normal> or
    <markup|wide-centered> instead.
  </explain>

  <\explain>
    <explain-macro|wide-normal|body>

    <explain-macro|wide-centered|body>
  <|explain>
    These tags are used to make the <src-arg|body> span over the entire
    paragraph width. The text is left-aligned in the case of
    <markup|wide-normal> and centered in the case of <markup|wide-centered>.
    Making a body span over the entire paragraph width does not change the
    rendering on paper, but it facilitates the editing on the document.
    Indeed, on the one hand side, the box which indicates that you are inside
    the environment will span over the entire paragraph width. On the other
    hand, when clicking sufficiently close to the text inside this box, it
    becomes easier to position your cursor at the start or at the end inside
    the environment. You may check this by clicking on one of the texts
    below:

    <with|color|red|\<gtr\>><wide-normal|Some text inside a
    <markup|wide-normal> environment.><with|color|red|\<less\>>

    <with|color|red|\<gtr\>><wide-centered|Some text inside a
    <markup|wide-centered> environment.><with|color|red|\<less\>>
  </explain>

  <\explain>
    <explain-macro|padded-normal|space-above|space-below|body>

    <explain-macro|padded-centered|space-above|space-below|body>
  <|explain>
    These tags are variants of <explain-macro|wide-normal|body> and
    <explain-macro|wide-centered|body>, which put some vertical white space
    <src-arg|space-above> and <src-arg|space-below> above and below the
    <src-arg|body>.
  </explain>

  <\explain>
    <explain-macro|wide-bothlined|top-border|bot-border|top-sep|bot-sep|body>

    <explain-macro|wide-std-bothlined|body>

    <explain-macro|padded-bothlined|space-above|space-below|top-border|bot-border|top-sep|bot-sep|body>

    <explain-macro|padded-std-bothlined|space-above|space-below|body>

    <explain-macro|wide-underlined|bborder|bsep|body>

    <explain-macro|wide-std-underlined|body>
  <|explain>
    These tags are used to make the <src-arg|body> span over the entire
    paragraph width and to put a horizontal rule above and/or below it. The
    widths of the rules are given by <src-arg|top-border> and
    <src-arg|bot-border> and the separation between the rules by
    <src-arg|top-sep> and <src-arg|bot-sep>. The standard width and
    separation (used by <markup|wide-std-bothlined>,
    <markup|padded-std-bothlined> and <markup|wide-std-underlined>) are
    <verbatim|1ln> and <verbatim|1sep>. The padded variants specify
    additional spaces <src-arg|space-above> and <src-arg|space-below> above
    and below the rules. As an example, <inactive*|<wide-std-underlined|left<htab|5mm>right>>
    yields:

    <wide-std-underlined|left<htab|5mm>right>

    Wide underlined environments are typically used for page headers. Wide
    environments which are both overlined and underlined are typically used
    for abstracts or floating figures and tables.
  </explain>

  <\explain>
    <explain-macro|wide-framed|border-width|hsep|vsep|body>

    <explain-macro|wide-std-framed|body>

    <explain-macro|wide-framed-colored|border-color|body-color|border-width|hsep|vsep|body>

    <explain-macro|wide-std-framed-colored|border-color|body-color|body>
  <|explain>
    These tags put the <src-arg|body> inside a frame box which spans over the
    whole paragraph. The user may specify a <src-arg|border-width>,
    horizontal and vertical separations <src-arg|hsep> and <src-arg|vsep>
    between the border and the text, and colors <src-arg|border-color> and
    <src-arg|body-color> for the border and the background. For instance,
    <inactive*|<wide-std-framed-colored|brown|pastel green|Hi there!>> yields

    <wide-std-framed-colored|brown|pastel green|Hi there!>
  </explain>

  <\explain>
    <explain-macro|indent-left|left-amount|body>

    <explain-macro|indent-right|right-amount|body>

    <explain-macro|indent-both|left-amount|right-amount|body>
  <|explain>
    These environments may be used in order to increase the current left
    and/or right indentation by the amounts <src-arg|left-amount> and/or
    <src-arg|right-amount>.
  </explain>

  <label|header-footer-helper>The following macros may be used in order to
  set headers and footers:

  <\explain|<explain-macro|set-header|header-text>>
    A macro for permanently changing the header. Notice that certain tags in
    the style file, like sectional tags, may override such manual changes.
  </explain>

  <\explain|<explain-macro|set-footer|footer-text>>
    A macro for permanently changing the footer. Again, certain tags in the
    style file may override such manual changes.
  </explain>

  <\explain|<explain-macro|blanc-page>>
    Remove all headers and footers from this page.
  </explain>

  <\explain|<explain-macro|simple-page>>
    Remove the header of this page and set the footer to the current page
    number (centered). This macro is often called for title pages or at the
    start of new chapters.
  </explain>

  Other macros provided by <tmdtd|std-utils> are:

  <\explain|<explain-macro|localize|text>>
    This macro should be used in order to ``localize'' some English text to
    the current language. For instance, <inactive*|<with|language|french|<localize|Theorem>>>
    yields <with|language|french|<localize|Theorem>>.
  </explain>

  <\explain|<explain-macro|map|fun|tuple>>
    This macro applies the macro <src-arg|fun> to each of the entries in a
    <src-arg|tuple> (or the children of an arbitrary <TeXmacs> tag) and
    returns the result as a tuple. For instance,
    <inactive*|<map|<macro|x|<em|<arg|x>>>|<tuple|1|2|3>>> yields
    <map|<macro|x|<em|<arg|x>>>|<tuple|1|2|3>> (the quote only appears when
    rendering the result, not when performing further computations with it).
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