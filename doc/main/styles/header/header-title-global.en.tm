<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing the global rendering of titles>

  Depending on the kind of attributes, complex titles often use several
  rendering styles in a simultaneous version. More precisely, a title usually
  consists of the following parts:

  <\itemize>
    <item>A well visible part at the top of the title page.

    <item>Additional notes, which are displayed in the footer.

    <item>An potentially invisible part, with information like running titles
    and authors.

    <item>A postponed part, which is only rendered in the abstract.
  </itemize>

  Similarly, individual authors may also contain a main part, which is
  rendered as part of the title, and an additional part, which is rendered as
  a footnote. Moreover, the layout often changes if the paper has more than
  one author.

  The <TeXmacs> mechanism for rendering titles therefore relies on several
  macros which extract the information corresponding to each of the above
  parts. This process may also involve some sorting, like putting the authors
  before the date or <em|vice versa>. At a second stage, each extracted part
  of the title is passed to the appropriate rendering macro. The following
  macros are used for extracting title information:

  <\explain>
    <explain-macro|doc-data-main|data-1|<with|mode|math|\<cdots\>>|data-n>

    <explain-macro|doc-data-main*|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    This macro only keeps and sorts the data which should be displayed in the
    main title. The <markup|doc-data-main*> variant is used in the case when
    the document has more than one author.
  </explain>

  <\explain>
    <explain-macro|doc-data-note|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    This macro only keeps and sorts the data which should be displayed as a
    footnote.
  </explain>

  <\explain>
    <explain-macro|doc-data-abstract|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    This macro only keeps and sorts the data which should be displayed in the
    abstract.
  </explain>

  <\explain>
    <explain-macro|doc-data-hidden|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    This macro only keeps and sorts the data which might or should not be
    displayed at all.
  </explain>

  In a similar fashion, the following macros are used for extracting author
  information:

  <\explain>
    <explain-macro|doc-author-main|<with|font-shape|right|<explain-macro|doc-author-data|data-1|<with|mode|math|\<cdots\>>|data-n>>>
  <|explain>
    This macro only keeps and sorts the data which should be displayed inside
    the main title.
  </explain>

  <\explain>
    <explain-macro|doc-author-note|data-1|<with|mode|math|\<cdots\>>|data-n>
  <|explain>
    This macro only keeps and sorts the data which should be displayed as a
    footnote.
  </explain>

  It should be noticed that each of the above macros should return a
  <markup|document> tag with the selected data as its children. For instance,

  <\tm-fragment>
    <inactive*|<style-with|src-compact|none|<doc-author-main|<author-address|Somewhere
    in Africa>|<author-name|The big GNU>|<author-note|Very hairy indeed!>>>>
  </tm-fragment>

  should typically return

  <\tm-fragment>
    <with|src-special|raw|<inactive*|<\style-with|src-compact|none>
      <author-address|Somewhere in Africa>

      <author-name|The big GNU>
    </style-with>>>
  </tm-fragment>

  The only exception to this rule is <markup|doc-data-hidden> which should
  return a <markup|concat> tag instead.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>