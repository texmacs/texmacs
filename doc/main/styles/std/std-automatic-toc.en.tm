<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Tables of contents>

  The following macros may be used in the main text for adding entries to the
  table of contents. They are automatically called by most sectional macros,
  but it is sometimes desirable to manually add additional entries.

  <\explain>
    <explain-macro|toc-main-1|entry>

    <explain-macro|toc-main-2|entry>
  <|explain>
    Create an important <src-arg|entry> in the table of contents. The macro
    <markup|toc-main-1> is intended to be used only for very important
    entries, such as parts of a book; it usually has to be added manually.
    The macro <markup|toc-main-2> is intended to be used for chapter or
    sections. Important entries are usually displayed in a strong font.
  </explain>

  <\explain>
    <explain-macro|toc-normal-1|entry>

    <explain-macro|toc-normal-2|entry>

    <explain-macro|toc-normal-3|entry>
  <|explain>
    Add a normal <src-arg|entry> to the table of contents, of different
    levels of importance. Usually, <markup|toc-normal-1> corresponds to
    sections, <markup|toc-normal-2> to subsections and <markup|toc-normal-3>
    to subsubsections.
  </explain>

  <\explain>
    <explain-macro|toc-small-1|entry>

    <explain-macro|toc-small-2|entry>
  <|explain>
    Add an unimportant <src-arg|entry> to the table of contents, like a
    paragraph. Since such entries are not very important, some styles may
    simply ignore the <markup|toc-small-1> and <markup|toc-small-2> tags.
  </explain>

  By redefining the following macros, it is possible to customize the
  rendering of tables of contents:

  <\explain>
    <explain-macro|toc-strong-1|content|where>

    <explain-macro|toc-strong-2|content|where>
  <|explain>
    Used for rendering table of contents entries created using
    <markup|toc-main-1> <abbr|resp.> <markup|toc-main-2>.
  </explain>

  <\explain>
    <explain-macro|toc-1|content|where>

    <explain-macro|toc-2|content|where>

    <explain-macro|toc-3|content|where>

    <explain-macro|toc-4|content|where>

    <explain-macro|toc-5|content|where>
  <|explain>
    Used for rendering table of contents entries created using
    <markup|toc-normal-1>, <markup|toc-normal-2>,
    <markup|toc-<no-break>normal-<no-break>3>, <markup|toc-small-1>
    <abbr|resp.> <markup|toc-small-2>.
  </explain>

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