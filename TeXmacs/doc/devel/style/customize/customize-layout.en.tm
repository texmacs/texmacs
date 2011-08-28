<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing the general layout>

  The general layout of a document is mainly modified by setting the
  appropriate environment variables for <hyper-link|page
  layout|../../format/environment/env-page.en.tm> and <hyper-link|paragraph
  layout|../../format/environment/env-par.en.tm>. For instance, by including
  the following lines in your style file, you can set the page size to
  <verbatim|letter> and the left and right margins to <verbatim|2in>:

  <\tm-fragment>
    <\inactive*>
      <assign|page-type|letter>

      <assign|page-odd|2in>

      <assign|page-even|2in>

      <assign|page-right|2in>
    </inactive*>
  </tm-fragment>

  It should be noticed that the environment variables for page layout are
  quite different in <TeXmacs> and <TeX>/<LaTeX>. In order to make it easier
  to adapt <LaTeX> style files to <TeXmacs>, we have therefore provided the
  <tmpackage|std-latex> package, which emulates the environment variables
  from <TeX>/<LaTeX>. Typically, this allows you determine the global layout
  by lines like

  <\tm-fragment>
    <\inactive*>
      <assign|tex-odd-side-margin|<macro|20pt>>

      <assign|tex-even-side-margin|<macro|20pt>>

      <assign|tex-text-width|<macro|33pc>>
    </inactive*>
  </tm-fragment>

  We notice that macros which return lengths are considered as
  <hyper-link|lengths|../../format/basics/lengths.en.tm> themselves. In the
  case of the <TeX>/<LaTeX> emulation package, we actually <em|require> all
  lengths to be macros.

  The page headers and footers are usually not determined by global
  environment variables or macros, since they may change when a new chapter
  or section is started. Instead, <TeXmacs> provides the call-back macros
  <markup|header-title>, <markup|header-author>, <markup|header-primary> and
  <markup|header-secondary>. These macros are called when the document title
  or author are specified or when a new primary or secondary section is
  started (primary sections are typically chapters in books, or sections in
  articles). For instance, the following redefinition makes the principal
  section name appear on even pages, together with the current page number
  and a wide underline.

  <\tm-fragment>
    <inactive*|<assign|header-primary|<macro|title|nr|type|<assign|page-even-header|<quasiquote|<style-with|src-compact|none|<wide-std-underlined|<style-with|src-compact|none|<page-the-page><htab|5mm><unquote|<arg|title>>>>>>>>>>
  </tm-fragment>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>