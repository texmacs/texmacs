<TeXmacs|1.0.3.4>

<style|tmdoc>

<\body>
  <tmdoc-title|<LaTeX> style sections>

  The <tmdtd|section-base> <abbr|d.t.d.> provides the standard tags for
  sections, which are the same as in <LaTeX>. Most sectional tags take one
  argument: the name of the section. The intention of the following tags is
  to produce numbered sections:

  <\description>
    <item*|<markup|chapter>>Macro for producing a numbered chapter title.

    <item*|<markup|section>>Macro for producing a numbered section title.

    <item*|<markup|subsection>>Macro for producing a numbered subsection
    title.

    <item*|<markup|subsubsection>>Macro for producing a numbered
    subsubsection title.

    <item*|<markup|paragraph>>Macro for producing a numbered paragraph title.

    <item*|<markup|subparagraph>>Macro for producing a numbered subparagraph
    title.

    <item*|<markup|appendix>>A variant of <markup|chapter> or
    <markup|section> for producing appendices.
  </description>

  Notice that the numbering is not required, but merely an intention: the
  <markup|paragraph> and <markup|subparagraph> tags are unsually not numbered
  and some styles (like the generic style) do not produce numbers at all. The
  tags <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*>, <markup|subparagraph*> and
  <markup|appendix*> can be used for producing the unnumbered variants of the
  above tags.

  By default, all sectional only produce the section title. When using the
  experimental package <tmpackage|structured-section>, all sectional tags are
  enriched, so that they take the body of the section as an optional
  argument. Moreover, an additional tag <markup|rsection> is provided in
  order to produce recursively embedded sections. For instance, an
  <markup|rsection> inside a <markup|section> behaves like a
  <markup|subsection>. In the future, all list items should become
  structured.

  The <tmdtd|section-base> <abbr|d.t.d.> also provides the following
  sectional environments with automatically generated content

  <\description>
    <item*|<markup|bibliography>>A macro with four arguments <verbatim|aux>,
    <verbatim|style>, <verbatim|file-name>, <verbatim|body>. The first
    argument <verbatim|aux> specifies the auxiliary channel with the data for
    generating the bibliography (<verbatim|bib>, by default). The arguments
    <verbatim|style> and <verbatim|file-name> contain the bibliography style
    and the file with the bibliographic database. The last argument
    corresponds to the automatically generated content.

    <item*|<markup|table-of-contents>>A macro with two arguments: the
    auxiliary channel with the data for generating the table of contents
    (<verbatim|toc>, by default), and the automatically generated content.

    <item*|<markup|the-index>>A macro with two arguments: the auxiliary
    channel with the data for generating the index (<verbatim|idx>, by
    default), and the automatically generated content.

    <item*|<markup|the-glossary>>A macro with two arguments: the auxiliary
    channel with the data for generating the glossary (<verbatim|gly>, by
    default), and the automatically generated content.
  </description>

  The above tags also admit the variants <markup|bibliography*>,
  <markup|table-of-contents*>, <markup|the-index*> and <markup|the-glossary*>
  with an additional argument before <verbatim|body>, which specifies the
  name of the section. For instance, the <markup|the-glossary*> tag is used
  for lists of figures and lists of tables.

  The <tmdtd|section-base> <abbr|d.t.d.> also contains many tags for
  customizing the rendering of sections and other section-related behaviour.
  The following two tags affect all sections:

  <\description>
    <item*|<markup|sectional-sep>>A macro for customizing the separator
    between the number of a section and its title. By default, we use two
    spaces.

    <item*|<markup|sectional-short-style>>A predicate which tells whether
    documents for this style are intended to be short or long. When
    <markup|sectional-short-style> evaluates to <verbatim|true>, then
    appendices, bibliographies, <abbr|etc.> are supposed to be special types
    of sections. Otherwise, they will be special types of chapters.
  </description>

  For each sectional tag <markup|<em|x>>, the following tags are provided for
  customization:

  <\description>
    <item*|<markup|<em|x>-title>>A macro for displaying the unnumbered
    section title.

    <item*|<markup|<em|x>-numbered-title>>A macro for displaying the numbered
    section title.

    <item*|<markup|<em|x>-display-numbers>>A predicate which specifies
    whether numbers will really be displayed. For instance, in the case of
    <markup|paragraph>, this macro evaluates to false. Consequently, even
    though <markup|<em|x>-numbered-title> <em|does> display the paragraph
    number, the main macro <markup|<em|x>> will call <markup|<em|x>-title>
    and not <markup|<em|x>-numbered-title>, so that paragraph titles are not
    numbered.

    <item*|<markup|<em|x>-sep>>A macro for customizing the separator between
    the number of a section and its title. By default, we call
    <markup|sectional-sep>.

    <item*|<markup|<em|x>-clean>>A hook for resetting all subcounters of the
    section.

    <item*|<markup|<em|x>-header>>A hook for changing the page headers.

    <item*|<markup|<em|x>-toc>>A hook for putting the section title into the
    table of contents.
  </description>

  Finally, the <tmdtd|section-base> <abbr|d.t.d.> provides rendering macros
  <markup|render-table-of-contents>, <markup|render-bibliography>,
  <markup|render-index> and <markup|render-glossary>, each of which takes two
  arguments: the name of the section and its body.

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