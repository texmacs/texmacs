<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Customization of the sectional tags>

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

  <\explain|<explain-macro|<em|x>-text>>
    A macro which displays the (localized) name of the sectional environment.
    For instance, <inactive*|<with|language|french|<appendix-text>>> produces
    ``<with|language|french|<appendix-text>>''.
  </explain>

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
  arguments: the name of the section and its body. It also provides the
  macros <markup|prologue-text>, <markup|epilogue-text>,
  <markup|bibliography-text>, <markup|table-of-contents-text>,
  <markup|index-text>, <markup|glossary-text>, <markup|list-of-figures-text>
  and <markup|list-of-tables-text> for customizing the names of special
  sections.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>