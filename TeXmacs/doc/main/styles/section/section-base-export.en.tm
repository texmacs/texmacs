<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Using sectional tags>

  The <tmdtd|section-base> <abbr|d.t.d.> provides the standard tags for
  sections, which are the same as in <LaTeX>. Most sectional tags take one
  argument: the name of the section. The intention of the following tags is
  to produce numbered sections:

  <\explain>
    <explain-macro|part|title>

    <explain-macro|chapter|title>

    <explain-macro|section|title>

    <explain-macro|subsection|title>

    <explain-macro|subsubsection|title>

    <explain-macro|paragraph|title>

    <explain-macro|subparagraph|title>

    <explain-macro|appendix|title>
  <|explain>
    The intention of this macro is to produce a numbered title for a part
    (<abbr|resp.> chapter, section, subsection, <abbr|etc.>). The numbering
    is not required, but merely an intention: the <markup|paragraph> and
    <markup|subparagraph> tags are usually not numbered and some styles (like
    the generic style) do not produce numbers at all.
  </explain>

  The tags <markup|part*>, <markup|chapter*>, <markup|section*>,
  <markup|subsection*>, <markup|subsubsection*>, <markup|paragraph*>,
  <markup|subparagraph*> and <markup|appendix*> can be used for producing the
  unnumbered variants of the above tags.

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

  <\explain>
    <explain-macro|the-glossary|aux|body>

    <explain-macro|list-of-figures|aux|body>

    <explain-macro|list-of-tables|aux|body>
  <|explain>
    Similar to <markup|table-of-contents> but for glossaries (default channel
    <verbatim|gly>), lists of figures (default channel <verbatim|figure>) and
    lists of tables (default channel <verbatim|table>).
  </explain>

  The above tags also admit the variants <markup|bibliography*>,
  <markup|table-of-contents*>, <markup|the-index*> and <markup|the-glossary*>
  with an additional argument <src-arg|name> before <src-arg|body>, which
  specifies the name of the section. For instance, the <markup|the-glossary*>
  was formerly used for lists of figures and lists of tables.

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