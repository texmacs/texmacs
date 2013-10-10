<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|<LaTeX> style sections>

  The <tmdtd|section-latex> <abbr|d.t.d.> provides the standard tags for
  sections, which are the same as in <LaTeX>. Most sectional tags take only
  one argument: the name of the section. In the future, we plan to provide
  alternative tags with two arguments, which will allow you to see the body
  of a section as part of the structure. The following tags usually yield
  numbered sections, which are referenced in the table of contents:

  <\explain|<markup|chapter>>
    Macro for producing a potentially numbered chapter title.
  </explain>

  <\explain|<markup|section>>
    Macro for producing a potentially numbered section title.
  </explain>

  <\explain|<markup|subsection>>
    Macro for producing a potentially numbered subsection title.
  </explain>

  <\explain|<markup|subsubsection>>
    Macro for producing a potentially numbered subsubsection title.
  </explain>

  <\explain|<markup|paragraph>>
    Macro for producing a potentially numbered paragraph title.
  </explain>

  <\explain|<markup|subparagraph>>
    Macro for producing a potentially numbered subparagraph title.
  </explain>

  The tags <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> and <markup|subparagraph*> can
  be used for producing the unnumbered variants of the above tags, which are
  not referenced in the table of contents. The <tmdtd|section-latex>
  <abbr|d.t.d.> also provides the following tags:

  <\explain|<markup|chapter**>>
    Macro with two arguments: a special type of chapter (like ``Epilogue'')
    and the name of the chapter.
  </explain>

  <\explain|<markup|appendix>>
    A variant of <markup|chapter> or <markup|section> for producing
    appendices.
  </explain>

  <\explain|<markup|section-sep>>
    A macro for customizing the separator between the number of a section and
    its title. By default, we use two spaces.
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
    <associate|language|portuguese>
  </collection>
</initial>