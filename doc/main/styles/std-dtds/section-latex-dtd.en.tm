<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<LaTeX> style sections>

  The <tmdtd|section-base> <abbr|d.t.d.> provides the standard tags for
  sections, which are the same as in <LaTeX>. Most sectional tags take one
  argument: the name of the section. The intention of the following tags is
  to produce numbered sections:

  <\explain>
    <explain-macro|chapter|title>

    <explain-macro|section|title>

    <explain-macro|subsection|title>

    <explain-macro|subsubsection|title>

    <explain-macro|paragraph|title>

    <explain-macro|subparagraph|title>

    <explain-macro|appendix|title>
  <|explain>
    The intention of this macro is to produce a numbered title for a chapter
    (<abbr|resp.> section, subsection, <abbr|etc.>). The numbering is not
    required, but merely an intention: the <markup|paragraph> and
    <markup|subparagraph> tags are usually not numbered and some styles (like
    the generic style) do not produce numbers at all.
  </explain>

  The tags <markup|chapter*>, <markup|section*>, <markup|subsection*>,
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

  <\explain|<explain-macro|bibliography|aux|style|file-name|body>>
    This macro is used for producing bibliographies. The first argument
    <src-arg|aux> specifies the auxiliary channel with the data for
    generating the bibliography (<verbatim|bib>, by default). The arguments
    <src-arg|style> and <src-arg|file-name> contain the bibliography style
    and the file with the bibliographic database. The <src-arg|body> argument
    corresponds to the automatically generated content.
  </explain>

  <\explain|<explain-macro|table-of-contents|aux|body>>
    This macro is used for producing tables of contents. The first argument
    <src-arg|aux> specifies the auxiliary channel with the data for
    generating the bibliography (<verbatim|toc>, by default). The
    <src-arg|body> argument corresponds to the automatically generated
    content.
  </explain>

  <\explain|<explain-macro|the-index|aux|body>>
    Similar to <markup|table-of-contents> but for indices and default channel
    <verbatim|idx>.
  </explain>

  <\explain|<explain-macro|the-glossary|aux|body>>
    Similar to <markup|table-of-contents> but for glossaries and default
    channel <verbatim|gly>.
  </explain>

  The above tags also admit the variants <markup|bibliography*>,
  <markup|table-of-contents*>, <markup|the-index*> and <markup|the-glossary*>
  with an additional argument <src-arg|name> before <src-arg|body>, which
  specifies the name of the section. For instance, the <markup|the-glossary*>
  tag is used for lists of figures and lists of tables.

  The <tmdtd|section-base> <abbr|d.t.d.> also contains many tags for
  customizing the rendering of sections and other section-related behaviour.
  The following two tags affect all sections:

  <\explain|<explain-macro|sectional-sep>>
    A macro for customizing the separator between the number of a section and
    its title. By default, we use two spaces.
  </explain>

  <\explain|<explain-macro|sectional-short-style>>
    A predicate which tells whether documents for this style are intended to
    be short or long. When <markup|sectional-short-style> evaluates to
    <verbatim|true>, then appendices, bibliographies, <abbr|etc.> are
    supposed to be special types of sections. Otherwise, they will be special
    types of chapters.
  </explain>

  For each sectional tag <markup|<em|x>>, the following tags are provided for
  customization:

  <\explain|<explain-macro|<em|x>-title|title>>
    A macro for displaying the unnumbered section title.
  </explain>

  <\explain|<explain-macro|<em|x>-numbered-title|title>>
    A macro for displaying the numbered section title.
  </explain>

  <\explain|<explain-macro|<em|x>-display-numbers>>
    A predicate which specifies whether numbers will really be displayed. For
    instance, in the case of <markup|paragraph>, this macro evaluates to
    false. Consequently, even though <markup|<em|x>-numbered-title> <em|does>
    display the paragraph number, the main macro <markup|<em|x>> will call
    <markup|<em|x>-title> and not <markup|<em|x>-numbered-title>, so that
    paragraph titles are not numbered.
  </explain>

  <\explain|<explain-macro|<em|x>-sep>>
    A macro for customizing the separator between the number of a section and
    its title. By default, we call <markup|sectional-sep>.
  </explain>

  <\explain|<explain-macro|<em|x>-clean>>
    A hook for resetting all subcounters of the section.
  </explain>

  <\explain|<explain-macro|<em|x>-header|name>>
    A hook for changing the page headers.
  </explain>

  <\explain|<explain-macro|<em|x>-toc|name>>
    A hook for putting the section title into the table of contents.
  </explain>

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