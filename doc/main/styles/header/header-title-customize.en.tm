<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing the rendering of title fields>

  Both title information and author information is rendered as a vertical
  stack of ``title blocks'' and ``author blocks''. The following macros may
  be used to customize the global rendering of such blocks:

  <\explain>
    <explain-macro|doc-title-block|content>

    <explain-macro|doc-author-block|content>
  <|explain>
    Macros for rendering one component of the title or author information.
  </explain>

  The following macros may be used to customize the rendering of title
  information; notice that they are usually built on top of
  <markup|doc-title-block>.

  <\explain|<explain-macro|doc-make-title|content>>
    This macro is used for the rendering of the main title information.
    Usually, it contains at least the title itself, as well as one or several
    authors.
  </explain>

  <\explain|<explain-macro|doc-render-title|title>>
    This macro is used for rendering the <src-arg|title> of the document. The
    <markup|doc-title> macro also takes care of rendering references to
    potential footnotes.
  </explain>

  <\explain|<explain-macro|doc-subtitle|title>>
    This macro is used for rendering the <src-arg|subtitle> of the document.
  </explain>

  <\explain|<explain-macro|doc-author|content>>
    In the case when the document has a single author, then this macro is
    used for rendering the <src-arg|content> information about him or her.
  </explain>

  <\explain|<explain-macro|doc-authors|content>>
    In the case when the document has several authors, then this macros is
    used for rendering all author-related <src-arg|content> which is part of
    the main title.
  </explain>

  <\explain|<explain-macro|doc-date|date>>
    This macro is used for rendering the creation <src-arg|date> of the
    document.
  </explain>

  The following macros may be used to customize the rendering of author
  information; notice that they are usually built on top of
  <markup|doc-author-block>.

  <\explain|<explain-macro|author-render-name|name>>
    Renders the <src-arg|name> of the author.The <markup|author-name> macro
    also takes care of rendering references to potential footnotes.
  </explain>

  <\explain|<explain-macro|author-by|name>>
    A macro which may put the text ``by '' in front of the <src-arg|name> of
    an author.
  </explain>

  <\explain|<explain-macro|author-address|address>>
    Renders the <src-arg|address> of the author.
  </explain>

  <\explain|<explain-macro|author-email|email>>
    Renders the <src-arg|email> address of the author.
  </explain>

  <\explain|<explain-macro|author-homepage|email>>
    Renders the <src-arg|homepage> of the author.
  </explain>

  The following macros are used for information which is usually not rendered
  as a part of the main title, but rather as a footnote or part of the
  abstract.

  <\explain>
    <explain-macro|doc-title-note|note>

    <explain-macro|doc-author-note|note>
  <|explain>
    A macro for rendering a <src-arg|note> attached to the document or one of
    its authors. The note will usually appear as part of a footnote. By
    default, notes that consist of several lines are compressed into a single
    paragraph.
  </explain>

  <\explain|<explain-macro|doc-keywords|kw-1|<with|mode|math|\<cdots\>>|kw-n>>
    A macro for displaying a list of keywords.
  </explain>

  <\explain|<explain-macro|doc-AMS-class|nr-1|<with|mode|math|\<cdots\>>|nr-n>>
    A macro for displaying a list of <abbr|A.M.S.> subject classification
    numbers.
  </explain>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>