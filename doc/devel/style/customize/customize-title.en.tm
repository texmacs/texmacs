<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing the treatment of title information>

  <TeXmacs> uses the <markup|doc-data> tag in order to specify global data
  for the document. These data are treated in two stages by the
  <markup|doc-data> macro. <hyper-link|First|../../../main/styles/header/header-title-global.en.tm>,
  the document data are separated into several categories, according to
  whether the data should be rendered as a part of the main title or in
  footnotes or the abstract. <hyper-link|Secondly|../../../main/styles/header/header-title-customize.en.tm>,
  the data in each category are rendered using suitable macros.

  Each child of the <markup|doc-data> is a tag with some specific information
  about the document. Currently implemented tags are <markup|doc-title>,
  <markup|doc-subtitle>, <markup|doc-author-data>, <markup|doc-date>,
  <markup|doc-running-title>, <markup|doc-running-author>,
  <markup|doc-keywords>, <markup|doc-AMS-class> and <markup|doc-note>. The
  <markup|doc-author-data> tag may occur several times and is used in order
  to specify data for each of the authors of the document. Each child of the
  <markup|doc-author-data> tag is a tag with information about the
  corresponding author. Currently implemented tags with author information
  are <markup|author-name>, <markup|author-address>, <markup|author-email>,
  <markup|author-homepage> and <markup|author-note>.

  Most of the tags listed above also correspond to macros for rendering the
  corresponding information as part of the main title. For instance, if the
  date should appear in bold italic at a distance of at least <verbatim|1fn>
  from the other title fields, then you may redefine <markup|doc-date> as

  <\tm-fragment>
    <\inactive*>
      <assign|doc-date|<macro|body|<style-with|src-compact|none|<vspace*|1fn><doc-title-block|<with|font-shape|italic|font-series|bold|<arg|body>>><vspace|1fn>>>>
    </inactive*>
  </tm-fragment>

  The <markup|title-block> macro is used in order to make the text span
  appropriately over the width of the title. The <markup|doc-title> and
  <markup|author-name> are special in the sense that they also render
  possible references to footnotes. For this reason, you should rather
  customize the <markup|doc-render-title> and <markup|author-render-name>
  macros in order to customize the rendering of the title and the name
  themselves.

  Notice also that the <markup|doc-running-title> and
  <markup|author-running-author> macros do not render anything, but rather
  call the <markup|header-title> and <markup|header-author> call-backs for
  setting the appropriate global page headers and footers. By default, the
  running title and author are extracted from the usual title and author
  names.

  In addition to the rendering macros which are present in the document, the
  main title (including author information, the date, <abbr|etc.>) is
  rendered using the <markup|doc-make-title> macro. The author information,
  as part of the main title, is rendered using <markup|doc-author> or
  <markup|doc-authors>, depending on whether the document has one or more
  authors. Footnotes to the title or to one of the authors are rendered using
  <markup|doc-title-note> <abbr|resp.> <markup|doc-author-note>. These
  footnote macros always expect a <markup|document> tag on input, because
  they may compress it into a horizontal concatenation.

  The first stage of processing the document data is more complex and the
  reader is invited to take a look at the <hyper-link|short
  descriptions|../../../main/styles/header/header-title-global.en.tm> of the
  macros which are involved in this process. It is also good to study the
  definitions of these macros in the <hyper-link|package
  itself|$TEXMACS_PATH/packages/header/title-base.ts>. In order to indicate
  the way things work, we finish with an example on how the email address and
  homepage of an author can be rendered in a footnote instead of the main
  title:

  <\tm-fragment>
    <\inactive*>
      <assign|doc-author-main|<\macro|data>
        <\quasi>
          <unquote*|<select|<quote-arg|data>|author-name>>

          <unquote*|<select|<quote-arg|data>|author-address>>
        </quasi>
      </macro>>

      <assign|doc-author-data-note|<xmacro|data|<\quasi>
        <unquote*|<select|<quote-arg|data>|author-email>>

        <unquote*|<select|<quote-arg|data>|author-homepage>>

        <unquote*|<select|<quote-arg|data>|author-note|document|<pat-any>>>
      </quasi>>>
    </inactive*>
  </tm-fragment>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>