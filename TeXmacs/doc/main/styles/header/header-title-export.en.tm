<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Entering titles and abstracts>

  The <tmdtd|header-title> <abbr|d.t.d.> provides tags for entering
  information about the entire document. The two top-level tags are

  <\explain|<explain-macro|doc-data|data-1|<math|\<cdots\>>|data-n>>
    Specify data attached to your document (title, authors, <abbr|etc.>; see
    below) and render the title.
  </explain>

  <\explain|<explain-macro|abstract|body>>
    The abstract for your paper.
  </explain>

  When creating a <markup|doc-data> tag using <menu|Insert|Title|Insert
  title>, <TeXmacs> automatically inserts a <markup|doc-title> tag as its
  first arguments. New data may be inserted from the <menu|Insert|Title>
  menu. Each child <src-arg|data-1>, <math|\<ldots\>>, <src-arg|data-n> of
  the <markup|doc-data> tag is of one of the following forms:

  <\explain|<explain-macro|doc-title|title>>
    Specify the <src-arg|title> of the document.
  </explain>

  <\explain|<explain-macro|doc-subtitle|subtitle>>
    Specify the <src-arg|subtitle> of the document.
  </explain>

  <\explain|<explain-macro|doc-author|data-1|<math|\<cdots\>>|data-n>>
    Specify datas for one of the authors of the document (name, affiliation,
    <abbr|etc.>; see below).
  </explain>

  <\explain|<explain-macro|doc-date|date>>
    The creation date of the document. In particular you may take
    <inactive*|<date>> for the value of <src-arg|date> for the current date.
  </explain>

  <\explain|<explain-macro|doc-running-title|title>>
    Specify a running <src-arg|title> for your document which may be used in
    page headers.
  </explain>

  <\explain|<explain-macro|doc-running-author|author>>
    Specify a running <src-arg|author> for your document which may be used in
    page headers.
  </explain>

  <\explain|<explain-macro|doc-keywords|kw-1|<math|\<cdots\>>|kw-n>>
    Specify keywords <src-arg|kw-1> until <src-arg|kw-n> for your document.
  </explain>

  <\explain|<explain-macro|doc-msc|nr-1|<math|\<cdots\>>|nr-n>>
    Specify <abbr|A.M.S.> subject classification numbers <src-arg|nr-1> until
    <src-arg|nr-n> for your document.
  </explain>

  <\explain|<explain-macro|doc-note|note>>
    A note about your document. In particular, you may take
    <inactive*|<with-TeXmacs-text>> for the value of <src-arg|note> in order
    to indicate that your document has been written using <TeXmacs>.
  </explain>

  <\explain|<explain-macro|author-data|data-1|<math|\<cdots\>>|data-n>>
    Specify structured datas for one of the authors of the document (name,
    affiliation, <abbr|etc.>; see below).
  </explain>

  When inserting an additional author using <menu|Insert|Title|Author|Insert
  author>, <TeXmacs> inserts a <explain-macro|doc-author|<with|font-shape|right|<explain-macro|author-data|...>>>
  tree with an <markup|author-name> tag as its first argument. New author
  data may be inserted from the <menu|Insert|Title|Author> menu. Each child
  <src-arg|data-1>, <math|\<ldots\>>, <src-arg|data-n> of the
  <markup|author-data> tag is of one of the following forms:

  <\explain|<explain-macro|author-name|name>>
    Specify the <src-arg|name> of the author.
  </explain>

  <\explain|<explain-macro|author-affiliation|affiliation>>
    The <src-arg|affiliation> of the author.
  </explain>

  <\explain|<explain-macro|author-email|email>>
    An <src-arg|email> address for the author.
  </explain>

  <\explain|<explain-macro|author-homepage|homepage>>
    The <src-arg|homepage> of the author.
  </explain>

  <\explain|<explain-macro|author-note|note>>
    A miscellaneous <src-arg|note> attached to the author, like a thank-word.
  </explain>

  As a general rule, the use of any of the subtags of <markup|doc-data> or
  <markup|author-data> is optional. An individual subtag may also be
  specified several times. This is useful for documents with several authors,
  or authors with several addresses. The rendering of title information is
  very style-dependent: some styles render addresses in a single line or even
  as a footnote, where other styles use a more widely spaced presentation.
  Often, some information like keywords or AMS subject classification numbers
  are only rendered as a part of the abstract.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>