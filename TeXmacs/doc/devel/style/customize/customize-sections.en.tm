<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing sectional tags>

  By default, <TeXmacs> provides the standard sectional tags from <LaTeX>
  <markup|part>, <markup|chapter>, <markup|section>, <markup|subsection>,
  <markup|subsubsection>, <markup|paragraph>, <markup|subparagraph>, as well
  as the special tag <markup|appendix>. <TeXmacs> also implements the
  unnumbered variants <markup|part*>, <markup|chapter*>, <abbr|etc.> and
  special section-like tags <markup|bibliography>,
  <markup|table-of-contents>, <markup|the-index>, <markup|the-glossary>,
  <markup|list-of-figures>, <markup|list-of-tables>.

  <\remark>
    <label|section-extra-argument-rem>Currently, the sectional tags take one
    argument, the section title, but a second argument with the body of the
    section is planned to be inserted in the future (see the experimental
    <tmpackage|structured-section> package). For this reason (among others),
    style files should never redefine the main sectional tags, but rather
    customize special macros which have been provided to this effect.
  </remark>

  From a global point of view, an important predicate macro is
  <markup|sectional-short-style>. When it evaluates to <verbatim|true>, then
  appendices, tables of contents, <abbr|etc.> are considered to be at the
  same level as sections. In the contrary case, they are at the same level as
  chapters. Typically, articles use the short sectional style whereas book
  use the long style.

  The rendering of a sectional tag <markup|<em|x>> is controlled through the
  macros <markup|<em|x>-sep>, <markup|<em|x>-title> and
  <markup|<em|x>-numbered-title>. The <markup|<em|x>-sep> macro prints the
  separator between the section number and the section title. It defaults to
  the macro <markup|sectional-sep>, which defaults in its turn to a wide
  space. For instance, after redefining

  <\tm-fragment>
    <inactive*|<assign|sectional-sep|<macro| -- >>>
  </tm-fragment>

  sectional titles would typically look like

  <\tm-fragment>
    <with|section-nr|1|sectional-sep|<macro| --
    >|<section-numbered-title|Hairy GNUs>>
  </tm-fragment>

  The <markup|<em|x>-title> and <markup|<em|x>-numbered-title> macros
  respectively specify how to render unnumbered and numbered section titles.
  Usually, the user only needs to modify <markup|<em|x>-title>, since
  <markup|<em|x>-numbered-title> is based on <markup|<em|x>-title>. However,
  if the numbers have to be rendered in a particular way, then it may be
  necessary to redefine <markup|<em|x>-numbered-title>. For instance,
  consider the redefinition

  <\tm-fragment>
    <inactive*|<assign|subsection-numbered-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<with|font-series|bold|<the-subsection>.
    ><arg|name>>>>>>
  </tm-fragment>

  This has the following effect on the rendering of subsection titles:

  <\tm-fragment>
    <with|section-nr|2|subsection-nr|3|the-subsection|<macro|<value|section-nr>.<value|subsection-nr>>|subsection-numbered-title|<macro|name|<sectional-normal|<with|font-series|bold|<the-subsection>.
    ><arg|name>>>|<subsection-numbered-title|Very hairy GNUs>>
  </tm-fragment>

  Notice that the <tmpackage|section-base> package provides several
  <hyper-link|useful helper macros|../../../main/styles/section/section-base-helper.en.tm>
  like <markup|sectional-normal>.

  <\remark>
    Sectional titles can either be rendered in a ``short'' or in the ``long''
    fashion. By default, paragraphs and subparagraphs use the short
    rendering, for which the body starts immediately at the right of the
    title:

    <\tm-fragment>
      <paragraph-title|My paragraph>Blah, blah, and more blahs...
    </tm-fragment>

    All other sectional tags use the long rendering, in which case the
    section title takes a separate line on its own:

    <\tm-fragment>
      <section-title|My section>

      Blah, blah, and more blahs...
    </tm-fragment>

    We do not recommend to modify the standard settings (<abbr|i.e.> to
    render paragraphs in a long way or sections in a short way). If you
    really want to do so, then we recommend to redefine the corresponding
    environment variables <inactive|<value|enrich-x-long>>. This will ensure
    upward compatibility when sectional tags will take an additional argument
    (see remark <reference|section-extra-argument-rem>).
  </remark>

  Besides their rendering, several other aspects of sectional tags can be
  customized:

  <\itemize>
    <item>The call-back macro <markup|<em|x>-clean> can be used for cleaning
    some counters when a new section is started. For instance, in order to
    prefix all standard environments by the section counter, you may use the
    following lines:

    <\tm-fragment>
      <\inactive*>
        <assign|section-clean|<macro|<reset-subsection><reset-std-env>>>

        <assign|display-std-env|<macro|nr|<section-prefix><arg|nr>>>
      </inactive*>
    </tm-fragment>

    <item>The call-back macro <markup|<em|x>-header> should be used in order
    to modify page headers and footers when a new section is started.
    Typically, this macro should call <markup|header-primary>, or
    <markup|header-secondary>, or do nothing.

    <item>The call-back macro <markup|<em|x>-toc> should be used in order to
    customize the way new sections appear in the table of contents.
  </itemize>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>