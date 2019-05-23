<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|section-base|1.0>

    <\src-purpose>
      The common base for most <TeXmacs> styles with sectional markup.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Environment parameters.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|<space|2spc>>>

  <assign|sectional-short-style|<macro|true>>

  <drd-props|sectional-sep|macro-parameter|regular>

  <\active*>
    <\src-comment>
      Names of special sections.
    </src-comment>
  </active*>

  <assign|prologue-text|<macro|<localize|Prologue>>>

  <assign|epilogue-text|<macro|<localize|Epilogue>>>

  <assign|bibliography-text|<macro|<localize|Bibliography>>>

  <assign|table-of-contents-text|<macro|<localize|Table of contents>>>

  <assign|index-text|<macro|<localize|Index>>>

  <assign|glossary-text|<macro|<localize|Glossary>>>

  <assign|list-of-figures-text|<macro|<localize|List of figures>>>

  <assign|list-of-tables-text|<macro|<localize|List of tables>>>

  <\active*>
    <\src-comment>
      Some useful macros on which the rendering of section titles can be
      based.
    </src-comment>
  </active*>

  <assign|sectional-short|<macro|name|<no-indent><arg|name>>>

  <assign|sectional-normal|<macro|name|<wide-normal|<arg|name><no-page-break><no-indent*>>>>

  <assign|sectional-centered|<macro|name|<wide-centered|<arg|name><no-page-break><no-indent*>>>>

  <assign|sectional-short-italic|<macro|name|<sectional-short|<with|font-shape|italic|<arg|name>>>>>

  <assign|sectional-normal-italic|<macro|name|<sectional-normal|<with|font-shape|italic|<arg|name>>>>>

  <assign|sectional-centered-italic|<macro|name|<sectional-centered|<with|font-shape|italic|<arg|name>>>>>

  <assign|sectional-short-bold|<macro|name|<style-with|src-compact|none|<sectional-short|<strong|<arg|name>>>>>>

  <assign|sectional-normal-bold|<macro|name|<style-with|src-compact|none|<sectional-normal|<strong|<arg|name>>>>>>

  <assign|sectional-centered-bold|<macro|name|<style-with|src-compact|none|<sectional-centered|<strong|<arg|name>>>>>>

  <assign|sectional-prefixed|<macro|prefix|name|<prefixed-line|<arg|prefix>|<arg|name>>>>

  <assign|tmhtml-sectional-prefixed|<macro|prefix|name|<arg|prefix><arg|name>>>

  <\active*>
    <\src-comment>
      The <verbatim|new-section> primitive is used for the definition of a
      new sectional tag. Each new sectional tag <verbatim|x> gives rise to a
      corresponding sectional counter and several additional environment
      variables:

      \ \ <verbatim|x>: for numbered sections.

      \ \ <verbatim|x*>: for unnumbered sections.

      \ \ <verbatim|x-text>: section text like Chapter, Section, etc. By the
      default, the first character of <verbatim|x> is upcased.

      \ \ <verbatim|x-numbered>: flag for knowing whether the last section is
      numbered or not.

      \ \ <verbatim|x-prefix>: macro for displaying the section number as a
      prefix (for subsection numbers).

      \ \ <verbatim|x-title>: a macro for displaying the title of the
      section.

      \ \ <verbatim|x-numbered-title>: a macro for displaying a numbered
      title.

      \ \ <verbatim|x-display-numbers>: predicate for testing whether the
      section numbers should be displayed.

      \ \ <verbatim|x-sep>: the separator between the number of a title and
      the title itself.

      \ \ <verbatim|x-clean>: a macro for additional cleaning up (used for
      resetting standard environments and <verbatim|appendix>).

      \ \ <verbatim|x-header>: a macro for setting the header.

      \ \ <verbatim|x-toc>: a macro for entering the section in the table of
      contents.

      All macros are set to reasonable defaults, but they may be customized
      afterwards. The flag <verbatim|x-numbered> and the macro
      <verbatim|x-prefix> are set in the bodies of <verbatim|x> and
      <verbatim|x*>, so they cannot be customized. Nevertheless, it may
      sometimes be useful to change the <verbatim|x-prefix> tag inside
      macros, as we do in the case of <verbatim|appendix>.
    </src-comment>
  </active*>

  <assign|new-section|<macro|x|<quasi|<style-with|src-compact|none|<new-counter|<unquote|<arg|x>>><assign|<unquote|<merge|<arg|x>|-text>>|<style-with|src-compact|none|<macro|<change-case|<localize|<unquote|<arg|x>>>|Upcase>>>><assign|<unquote|<arg|x>>|<macro|title|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|x>|-numbered>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|x>|-display-numbers>>>>><assign|<unquote|<merge|<arg|x>|-prefix>>|<style-with|src-compact|none|<macro|<compound|<unquote|<merge|the-|<arg|x>>>>.>>><compound|<unquote|<merge|next-|<arg|x>>>><compound|<unquote|<merge|<arg|x>|-clean>>><compound|<unquote|<merge|<arg|x>|-header>>|<arg|title>><compound|<unquote|<merge|<arg|x>|-toc>>|<arg|title>><style-with|src-compact|none|<if|<value|<unquote|<merge|<arg|x>|-numbered>>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|x>|-numbered-title>>|<arg|title>>>|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|x>|-title>>|<arg|title>>>>>>>><assign|<unquote|<merge|<arg|x>|*>>|<macro|title|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|x>|-numbered>>|false><assign|<unquote|<merge|<arg|x>|-prefix>>|<macro|>><compound|<unquote|<merge|<arg|x>|-clean>>><compound|<unquote|<merge|<arg|x>|-header>>|<arg|title>><compound|<unquote|<merge|<arg|x>|-toc>>|<arg|title>><style-with|src-compact|none|<compound|<unquote|<merge|<arg|x>|-title>>|<arg|title>>>>>><assign|<unquote|<merge|<arg|x>|-numbered>>|false><assign|<unquote|<merge|<arg|x>|-prefix>>|<macro|>><assign|<unquote|<merge|<arg|x>|-display-numbers>>|true><drd-props|<unquote|<merge|<arg|x>|-display-numbers>>|macro-parameter|boolean><assign|<unquote|<merge|<arg|x>|-sep>>|<macro|<sectional-sep>>><drd-props|<unquote|<merge|<arg|x>|-sep>>|macro-parameter|regular><assign|<unquote|<merge|<arg|x>|-title>>|<macro|title|<style-with|src-compact|none|<strong|<arg|title>><compound|<unquote|<merge|<arg|x>|-sep>>>>>><assign|<unquote|<merge|<arg|x>|-numbered-title>>|<macro|title|<style-with|src-compact|none|<compound|<unquote|<merge|<arg|x>|-title>>|<style-with|src-compact|none|<sectional-prefixed|<compound|<unquote|<merge|the-|<arg|x>>>><compound|<unquote|<merge|<arg|x>|-sep>>>|<arg|title>>>>>>><assign|<unquote|<merge|<arg|x>|-clean>>|<macro|>><assign|<unquote|<merge|<arg|x>|-header>>|<macro|title|>><assign|<unquote|<merge|<arg|x>|-toc>>|<macro|title|>>>>>>

  <\active*>
    <\src-comment>
      Define the standard sectional tags. Also link with the standard
      environments.
    </src-comment>
  </active*>

  <new-section|part>

  <new-section|chapter>

  <new-section|section>

  <new-section|subsection>

  <new-section|subsubsection>

  <new-section|paragraph>

  <new-section|subparagraph>

  \;

  <assign|display-part|<macro|nr|<number|<arg|nr>|Roman>>>

  <assign|display-section|<macro|nr|<if|<sectional-short-style>|<arg|nr>|<chapter-prefix><arg|nr>>>>

  <assign|display-subsection|<macro|nr|<section-prefix><arg|nr>>>

  <assign|display-subsubsection|<macro|nr|<subsection-prefix><arg|nr>>>

  <assign|display-paragraph|<macro|nr|<subsubsection-prefix><arg|nr>>>

  <assign|display-subparagraph|<macro|nr|<paragraph-prefix><arg|nr>>>

  <assign|paragraph-display-numbers|<macro|false>>

  <assign|subparagraph-display-numbers|<macro|false>>

  \;

  <assign|chapter-clean|<macro|<reset-section>>>

  <assign|section-clean|<macro|<reset-subsection>>>

  <assign|subsection-clean|<macro|<reset-subsubsection>>>

  <assign|subsubsection-clean|<macro|<reset-paragraph>>>

  <assign|paragraph-clean|<macro|<reset-subparagraph>>>

  <\active*>
    <\src-comment>
      Special treatment of appendices and other special sectional tags
    </src-comment>
  </active*>

  <new-section|appendix>

  <assign|display-appendix|<macro|nr|<style-with|src-compact|none|<if|<sectional-short-style>|<display-section|<number|<arg|nr>|Alpha>>|<display-chapter|<number|<arg|nr>|Alpha>>>>>>

  <assign|appendix-clean|<macro|<style-with|src-compact|none|<if|<sectional-short-style>|<style-with|src-compact|none|<reset-subsection><section-clean><assign|section-prefix|<value|appendix-prefix>>>|<style-with|src-compact|none|<reset-section><chapter-clean><assign|chapter-prefix|<value|appendix-prefix>>>>>>>

  <assign|appendix-title|<macro|title|<style-with|src-compact|none|<if|<sectional-short-style>|<section-title|<arg|title>>|<chapter-title|<arg|title>>>>>>

  \;

  <assign|principal-section|<macro|title|<style-with|src-compact|none|<if|<sectional-short-style>|<section|<arg|title>>|<chapter|<arg|title>>>>>>

  <assign|principal-section*|<macro|title|<style-with|src-compact|none|<if|<sectional-short-style>|<section*|<arg|title>>|<chapter*|<arg|title>>>>>>

  <assign|prologue|<macro|<principal-section*|<prologue-text>>>>

  <assign|epilogue|<macro|<principal-section*|<epilogue-text>>>>

  <\active*>
    <\src-comment>
      Hooks for the table of contents.
    </src-comment>
  </active*>

  <assign|toc-title|<macro|env|title|<style-with|src-compact|none|<if|<compound|<unquote|<merge|<arg|env>|-numbered>>>|<compound|<unquote|<merge|the-|<arg|env>>>><compound|<unquote|<merge|<arg|env>|-sep>>><arg|title>|<arg|title>>>>>

  <assign|part-toc|<macro|name|<toc-main-1|<toc-title|part|<arg|name>>>>>

  <assign|chapter-toc|<macro|name|<style-with|src-compact|none|<if|<sectional-short-style>|<toc-main-1|<toc-title|chapter|<arg|name>>>|<toc-main-2|<toc-title|chapter|<arg|name>>>>>>>

  <assign|section-toc|<macro|name|<style-with|src-compact|none|<if|<sectional-short-style>|<toc-main-2|<toc-title|section|<arg|name>>>|<toc-normal-1|<toc-title|section|<arg|name>>>>>>>

  <assign|subsection-toc|<macro|name|<toc-normal-2|<toc-title|subsection|<arg|name>>>>>

  <assign|subsubsection-toc|<macro|name|<toc-normal-3|<toc-title|subsubsection|<arg|name>>>>>

  <assign|paragraph-toc|<macro|name|<toc-small-1|<toc-title|paragraph|<arg|name>>>>>

  <assign|subparagraph-toc|<macro|name|<toc-small-2|<toc-title|subparagraph|<arg|name>>>>>

  <assign|appendix-toc|<macro|name|<style-with|src-compact|none|<toc-main-2|<appendix-text><if|<appendix-numbered>|
  <the-appendix>><appendix-sep><arg|name>>>>>

  <\active*>
    <\src-comment>
      Hooks for sectional headers.
    </src-comment>
  </active*>

  <assign|chapter-header|<macro|name|<style-with|src-compact|none|<if|<not|<sectional-short-style>>|<style-with|src-compact|none|<header-primary|<arg|name>|<if|<chapter-numbered>|<the-chapter>>|<chapter-text>>>>>>>

  <assign|section-header|<macro|name|<style-with|src-compact|none|<if|<sectional-short-style>|<style-with|src-compact|none|<header-primary|<arg|name>|<if|<section-numbered>|<the-section>>|<section-text>>>|<style-with|src-compact|none|<header-secondary|<arg|name>|<if|<section-numbered>|<the-section>>|<section-text>>>>>>>

  <assign|subsection-header|<macro|name|<style-with|src-compact|none|<if|<sectional-short-style>|<style-with|src-compact|none|<header-secondary|<arg|name>|<if|<subsection-numbered>|<the-subsection>>|<section-text>>>>>>>

  <assign|appendix-header|<macro|name|<style-with|src-compact|none|<header-primary|<arg|name>|<if|<appendix-numbered>|<the-appendix>>|<appendix-text>>>>>

  <\active*>
    <\src-comment>
      Rendering of sections with automatically generated content.
    </src-comment>
  </active*>

  <assign|render-bibliography|<\macro|name|body>
    <principal-section*|<arg|name>>

    <with|par-first|0fn|par-par-sep|0fn|font-size|0.84|<arg|body>>
  </macro>>

  <assign|render-table-of-contents|<\macro|name|body>
    <with|chapter-toc|<macro|name|>|section-toc|<macro|name|>|<principal-section*|<arg|name>>>

    <with|par-first|0fn|par-par-sep|0fn|<arg|body>>
  </macro>>

  <assign|render-index|<\macro|name|body>
    <\with|par-par-sep|-0.5fn>
      <principal-section*|<arg|name>>

      \;
    </with>

    <with|par-first|0fn|par-par-sep|0fn|font-size|0.84|par-columns|2|<arg|body>>
  </macro>>

  <assign|render-glossary|<\macro|name|body>
    <principal-section*|<arg|name>>

    <with|par-first|0fn|par-par-sep|0fn|font-size|0.84|<arg|body>>
  </macro>>

  <\active*>
    <\src-comment>
      Sections with automatically generated content. The *-forms generate
      content in the same way as the main form, but allow the use of another
      auxiliary channel and another name (c.f. lists of figures).
    </src-comment>
  </active*>

  <assign|bibliography|<\macro|aux|style|file-name|body>
    <render-bibliography|<bibliography-text>|<arg|body>>
  </macro>>

  <assign|bibliography*|<\macro|aux|style|file-name|name|body>
    <render-bibliography|<localize|<arg|name>>|<arg|body>>
  </macro>>

  <drd-props|bibliography|arity|4|identifier|0|string|1|url|2>

  <drd-props|bibliography*|arity|5|identifier|0|string|1|url|2>

  <assign|thebibliography|<\macro|largest|body>
    <render-bibliography|<bibliography-text>|<bib-list|<arg|largest>|<arg|body>>>
  </macro>>

  <assign|table-of-contents|<\macro|aux|body>
    <render-table-of-contents|<table-of-contents-text>|<arg|body>>
  </macro>>

  <assign|table-of-contents*|<\macro|aux|name|body>
    <render-table-of-contents|<localize|<arg|name>>|<arg|body>>
  </macro>>

  <assign|the-index|<\macro|aux|body>
    <render-index|<index-text>|<arg|body>>
  </macro>>

  <assign|the-index*|<\macro|aux|name|body>
    <render-index|<localize|<arg|name>>|<arg|body>>
  </macro>>

  <assign|the-glossary|<\macro|aux|body>
    <render-glossary|<glossary-text>|<arg|body>>
  </macro>>

  <assign|the-glossary*|<\macro|aux|name|body>
    <render-glossary|<localize|<arg|name>>|<arg|body>>
  </macro>>

  <assign|list-of-figures|<\macro|aux|body>
    <\the-glossary*|<arg|aux>|<list-of-figures-text>>
      <arg|body>
    </the-glossary*>
  </macro>>

  <assign|list-of-tables|<\macro|aux|body>
    <\the-glossary*|<arg|aux>|<list-of-tables-text>>
      <arg|body>
    </the-glossary*>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>