<TeXmacs|1.99.13>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|svmono|1.0>

      <\src-purpose>
        The Springer-Verlag Monograph style.
      </src-purpose>

      <\src-copyright|2005>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env-base|env-math|env-enunciation|env-float|env-program|header-book|title-base|section-book|std-latex>

  <assign|env-theorem-dtd|1.0>

  <active*|<\src-comment>
    TeX-like style parameters.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|63pt>>

  <assign|tex-even-side-margin|<macro|63pt>>

  <assign|tex-text-width|<macro|28pc>>

  \;

  <assign|tex-voffset|<macro|0pt>>

  <assign|tex-top-margin|<macro|0cm>>

  <assign|*tex-head-height|<macro|12pt>>

  <assign|tex-head-sep|<macro|12pt>>

  <assign|*tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|540pt>>

  <assign|*tex-foot-height-heuristic|<macro|1em>>

  <assign|*tex-foot-skip|<macro|30pt>>

  \;

  <assign|tex-footnote-sep|<macro|7.7pt>>

  <assign|*tex-footnote-tm-barlen|<macro|0.4par>>

  <assign|*tex-column-sep|<macro|1.5cc>>

  <assign|*tex-float-sep|<macro|<tmlen|10pt|12pt|14pt>>>

  <assign|tex-margin-par-width|<macro|90pt>>

  <assign|*tex-margin-par-sep|<macro|10pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|10>

  <assign|par-first|15pt>

  <assign|par-line-sep|<macro|<tex-len|0pt|1pt|0pt>>>

  <active*|<src-short-comment|hfuzz and arraycolsep?>>

  \;

  <assign|*tex-jot|<macro|2pt>>

  <assign|*tex-above-display-skip|<macro|<tex-len|3mm|6pt|4pt>>>

  <assign|*tex-below-display-skip|<macro|<tex-len|3mm|6pt|4pt>>>

  <assign|*tex-above-display-short-skip|<macro|<tex-len|1mm|6pt|0pt>>>

  <assign|*tex-below-display-short-skip|<macro|<tex-len|2mm|4pt|4pt>>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-base-size|5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|7|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<small|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-base-size|9|par-sep|2pt|tex-above-display-skip|<macro|<tex-len|8.5pt|3pt|4pt>>|tex-below-display-skip|<macro|<tex-len|8.5pt|3pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|4pt|2pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-base-size|10|par-sep|0.2em|tex-above-display-skip|<macro|<macro|0.75fn>>|tex-below-display-skip|<macro|<macro|0.75fn>>|tex-above-display-short-skip|<macro|<macro|0.15fn>>|tex-below-display-short-skip|<macro|<macro|0.15fn>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-base-size|12|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|14|par-sep|<minus|<tmlen|16dd>|<tmlen|14pt>>|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|17|par-sep|<minus|<tmlen|17dd>|<tmlen|17pt>>|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|20|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-base-size|25|par-sep|5pt|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|part-size|<macro|x|<larger|<arg|x>>>>

  <assign|part-style|<macro|x|<with|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|chap-size|<macro|x|<larger|<arg|x>>>>

  <assign|chap-style|<macro|x|<with|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|sec-size|<macro|x|<large|<arg|x>>>>

  <assign|sec-style|<macro|x|<with|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|subsec-size|<macro|x|<normal-size|<arg|x>>>>

  <assign|subsec-style|<macro|x|<with|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|enrich-paragraph-long|true>

  \;

  <assign|part-number-title|<macro|num|title|<style-with|src-compact|none|<new-dpage*><blanc-page><no-indent><new-line><no-indent><wide-std-underlined|<chap-style|<chap-size|<htab|0mm><part-text><arg|num>>>><vspace|106pt><new-line><no-indent><part-style|<part-size|<htab|0mm><arg|title>>><vspace|10pt><no-page-break><no-indent*><right-flush>>>>

  <assign|part-title|<macro|title|<part-number-title||<arg|title>>>>

  <assign|part-numbered-title|<macro|title|<part-number-title|
  <the-part>|<arg|title>>>>

  \;

  <assign|chapter-number-title|<macro|num|title|<style-with|src-compact|none|<new-dpage*><no-indent><new-line><no-indent><wide-std-underlined|<chap-style|<chap-size|<arg|num>>>><vspace|10pt><new-line><no-indent><chap-style|<chap-size|<arg|title>>><vspace|106pt><no-page-break><no-indent*><right-flush>>>>

  <assign|tmhtml-chapter-number-title|<macro|num|title|<chapter-title|<arg|num><space|0.5em><arg|title>>>>

  <assign|chapter-title|<macro|title|<chapter-number-title||<arg|title>>>>

  <assign|chapter-numbered-title|<macro|title|<chapter-number-title|<the-chapter>|<arg|title>>>>

  <assign|appendic-numbered-title|<macro|title|<style-with|src-compact|none|<chapter-number-title|<appendix-text>
  <the-appendix>|<arg|title>>>>>

  \;

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<style-with|src-compact|none|<vspace*|<tex-len|24pt|4pt|4pt>><sec-style|<sec-size|<arg|name>>><vspace|<tex-len|12pt|4pt|4pt>>>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<style-with|src-compact|none|<vspace*|<tex-len|17pt|4pt|4pt>><subsec-style|<subsec-size|<arg|name>>><vspace|<tex-len|10pt|4pt|4pt>>>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<style-with|src-compact|none|<vspace*|<tex-len|17pt|4pt|4pt>><subsec-style|<normal-size|<arg|name>>><vspace|<tex-len|10pt|4pt|4pt>>>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<style-with|src-compact|none|<vspace*|<tex-len|10pt|4pt|4pt>><normal-size|<arg|name>><vspace|<tex-len|10pt|4pt|4pt>>>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<style-with|src-compact|none|<vspace*|<tex-len|5.388pt|4pt|4pt>><normal-size|<arg|name>><space|5pt>>>>>>

  <active*|<\src-comment>
    Section and environment numbering.
  </src-comment>>

  <assign|between-number-space|<macro|5pt>>

  <assign|sectional-sep|<macro|<space|<between-number-space>>>>

  <assign|tmhtml-sectional-sep|<macro|<space|0.5em>>>

  <assign|*part-sep|<macro|<macro|7pt>>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|true>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<style-with|src-compact|none|<small|<no-indent><htab|0mm><arg|s><hspace|<tmlen|0.5cc|2.5cc|2.5cc>><page-number>>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<style-with|src-compact|none|<small|<no-indent><page-number><hspace|<tmlen|0.5cc|2.5cc|2.5cc>><arg|s>>>>>>

  \;

  <assign|header-title|<macro|name|<blanc-page>>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|<style-with|src-compact|none|<blanc-page><odd-page-text|<arg|nr><space|<between-number-space>><arg|name>><even-page-text|<arg|nr><space|<between-number-space>><arg|name>>>>>

  <assign|header-secondary|<macro|name|nr|what|<style-with|src-compact|none|<odd-page-text|<arg|nr><space|<between-number-space>><arg|name>>>>>

  <active*|<\src-comment>
    Theorem-like environemments rendering.
  </src-comment>>

  <group-common-counter|theorem-env>

  <assign|env-number|<macro|num|<arg|num>>>

  <assign|enunciation-sep|<macro|. >>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>

  <assign|smart-qed|<macro|>>

  \;

  <assign|render-enunciation|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<arg|which>||<arg|body>>>
  </macro>>

  <assign|render-exercise|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<exercise-name|<arg|which><exercise-sep>>||<arg|body>>>
  </macro>>

  <assign|render-proof|<\macro|which|body>
    <\surround||<smart-qed>>
      <\render-remark|<arg|which>>
        <arg|body>
      </render-remark>
    </surround>
  </macro>>

  <active*|<\src-comment>
    Theorem-like environments.
  </src-comment>>

  <new-theorem|theorem|Theorem>

  <new-theorem|corollary|Corollary>

  <new-theorem|definition|Definition>

  <new-theorem|lemma|Lemma>

  <new-theorem|proposition|Proposition>

  \;

  <new-exercise|exercise|Exercise>

  <new-exercise|problem|Problem>

  <new-exercise|solution|Solution>

  \;

  <new-remark|math-case|Case>

  <new-remark|conjecture|Conjecture>

  <new-remark|example|Example>

  <new-remark|note|Note>

  <new-remark|property|Property>

  <new-remark|question|Question>

  <new-remark|remark|Remark>

  \;

  <active*|<src-short-comment|<TeXmacs> environments>>

  <new-theorem|axiom|Axiom>

  <new-theorem|notation|Notation>

  <new-remark|warning|Warning>

  <\active*>
    <\src-comment>
      List environments.
    </src-comment>
  </active*>

  <assign|simple-item|<macro|x|<style-with|src-compact|none|<with|par-first|-15pt|<yes-indent>><resize|<arg|x>|||15pt|>>>>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<with|par-first|-15pt|<yes-indent>><resize|<arg|x>
  |<minus|1r|15pt>||<plus|1r|0pt>|>>>>

  <assign|compact-item|<macro|x|<style-with|src-compact|none|<with|par-first|-15pt|<yes-indent>><resize|<arg|x>|||<maximum|1r|15pt>|>>>>

  \;

  <assign|list-level|0>

  <assign|render-list|<\macro|body>
    <\padded-normal|<if|<equal|<value|list-level>|0>|0.5fn|0fn>|<if|<equal|<value|list-level>|0>|0.5fn|0fn>>
      <\indent-left|15pt>
        <\surround|<no-page-break*>|<no-indent*>>
          <\with|list-level|<plus|<mod|<value|list-level>|3>|1>>
            <arg|body>
          </with>
        </surround>
      </indent-left>
    </padded-normal>
  </macro>>

  \;

  <new-list|itemize-1|<value|simple-item>|<macro|x|<active*|<with|mode|math|<rigid|\<bullet\>>>>>>

  <new-list|itemize-2|<value|simple-item>|<macro|x|<active*|<with|mode|math|<rigid|->>>>>

  <new-list|itemize-3|<value|simple-item>|<macro|x|.>>

  <new-list|itemize-minus|<value|simple-item>|<macro|x|<active*|<with|mode|math|<rigid|->>>>>

  <new-list|itemize-dot|<value|simple-item>|<macro|x|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-arrow|<value|simple-item>|<macro|x|<active*|<with|mode|math|<rigid|\<rightarrow\>>>>>>

  \;

  <new-list|enumerate-1|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-2|<value|aligned-bracket-item>|<macro|x|<number|<arg|x>|alpha>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>

  \;

  <assign|compact-dash-item|<macro|x|<compact-item|<arg|x> <emdash> >>>

  <assign|long-compact-space-item|<macro|x|<item-long|<compact-space-item|<arg|x>>>>>

  <new-list|description-compact|<value|compact-space-item>|<macro|x|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-aligned|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-dash|<value|compact-dash-item>|<macro|x|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-long|<value|long-compact-space-item>|<macro|x|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description|<value|compact-space-item>|<macro|x|<active*|<with|mode|math|<with|font-series|bold|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <\active*>
    <\src-comment>
      Customization of other environments.
    </src-comment>
  </active*>

  <assign|figure-text|<macro|<localize|Fig.>>>

  <assign|bibliography-text|<macro|<localize|References>>>

  <assign|table-of-contents-text|<macro|<localize|Table of Contents>>>

  <assign|list-of-figures-text|<macro|<localize|List of Figures>>>

  <assign|list-of-tables-text|<macro|<localize|List of Tables>>>

  <\active*>
    <\src-comment>
      Title and author information.
    </src-comment>
  </active*>

  The title page and author information should be typeset manually.

  <active*|<\src-comment>
    Tables of contents.
  </src-comment>>

  <assign|chapter-toc-width|20pt>

  <assign|section-toc-width|22.5pt>

  <assign|subsection-toc-width|30.5pt>

  <assign|subsubsection-toc-width|38pt>

  <assign|paragraph-toc-width|45pt>

  <assign|subparagraph-toc-width|53pt>

  <assign|section-toc-indent|<value|chapter-toc-width>>

  <assign|subsection-toc-indent|<plus|<value|section-toc-indent>|<value|section-toc-width>>>

  <assign|subsubsection-toc-indent|<plus|<value|subsection-toc-indent>|<value|subsection-toc-width>>>

  <assign|paragraph-toc-indent|<plus|<value|subsubsection-toc-indent>|<value|subsubsection-toc-width>>>

  <assign|subparagraph-toc-indent|<plus|<value|paragraph-toc-indent>|<value|paragraph-toc-width>>>

  <assign|subsubparagraph-toc-indent|<plus|<value|subparagraph-toc-indent>|<value|subparagraph-toc-width>>>

  \;

  <assign|toc-title|<macro|env|title|<style-with|src-compact|none|<if|<compound|<unquote|<merge|<arg|env>|-numbered>>>|<style-with|src-compact|none|<if|<equal|<arg|env>|part>|<style-with|src-compact|none|<part-text>
  <compound|<unquote|<merge|the-|<arg|env>>>><space|7pt><arg|title>>|<style-with|src-compact|none|<style-with|src-compact|none|<resize|<compound|<unquote|<merge|the-|<arg|env>>>>|||<value|<unquote|<merge|<arg|env>|-toc-width>>>|>><arg|title>>>>|<arg|title>>>>>

  \;

  <assign|toc-small-1|<macro|what|>>

  <assign|toc-small-2|<macro|what|>>

  <assign|render-table-of-contents|<\macro|name|body>
    <with|chapter-toc|<macro|name|>|<principal-section*|<arg|name>>>

    <with|par-first|0fn|par-par-sep|0fn|<arg|body>>
  </macro>>

  \;

  <assign|toc-sv|<macro|l|w|left|right|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hpart|1>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-valign|b>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|1|2|2|cell-width|1.8em>|<cwith|1|-1|1|-1|cell-lsep|0cm>|<cwith|1|-1|1|-1|cell-rsep|0cm>|<cwith|1|1|1|1|cell-halign|L>|<table|<row|<\cell>
    <\with|par-left|<arg|l>|par-first|<minus|<arg|w>>|par-mode|left>
      <yes-indent><arg|left><toc-dots>
    </with>
  </cell>|<cell|<arg|right>>>>>>>>

  <assign|toc-strong-1|<macro|left|right|<quasiquote|<surround|<vspace*|2em>|<vspace|5pt>|<wide-bothlined|1ln|1ln|3sep|3sep|<with|font-series|bold|math-font-series|bold|<style-with|src-compact|none|<unquote|<arg|left>>>>>>>>>

  <assign|toc-strong-2|<macro|left|right|<style-with|src-compact|none|<vspace*|1em><style-with|src-compact|none|<toc-sv|<value|section-toc-indent>|<value|chapter-toc-width>|<with|font-series|bold|math-font-series|bold|<arg|left>>|<arg|right>>>>>>

  <assign|toc-1|<macro|left|right|<toc-sv|<value|subsection-toc-indent>|<value|section-toc-width>|<arg|left>|<arg|right>>>>

  <assign|toc-2|<macro|left|right|<toc-sv|<value|subsubsection-toc-indent>|<value|subsection-toc-width>|<arg|left>|<arg|right>>>>

  <assign|toc-3|<macro|left|right|<toc-sv|<value|paragraph-toc-indent>|<value|subsubsection-toc-width>|<arg|left>|<arg|right>>>>

  <assign|toc-4|<macro|left|right|<toc-sv|<value|subparagraph-toc-indent>|<value|paragraph-toc-width>|<arg|left>|<arg|right>>>>

  <assign|toc-5|<macro|left|right|<style-with|src-compact|none|<toc-sv|<value|subsubparagraph-toc-indent>|<value|subparagraph-toc-width>|<arg|left>|<arg|right>>>>>

  <\active*>
    <\src-comment>
      Bibliographies.
    </src-comment>
  </active*>

  <assign|transform-bibitem|<macro|x|<arg|x> >>

  <assign|render-bibitem|<macro|text|<style-with|src-compact|none|<with|par-first|<minus|1tmpt|<value|bibitem-width>>|<yes-indent>><resize|<arg|text>|||<maximum|1r|<value|bibitem-width>>|>>>>

  <assign|bib-list|<\macro|largest|body>
    <\with|bibitem-width|<box-info|<transform-bibitem|<arg|largest>>|w.>|render-list|<value|render-bib-list>|bibitem-nr|0>
      <\description>
        <arg|body>
      </description>
    </with>
  </macro>>

  <assign|render-bibliography|<\macro|name|body>
    <principal-section*|<arg|name>>

    <\small>
      <\with|par-first|0fn|par-par-sep|0fn>
        <arg|body>
      </with>
    </small>
  </macro>>

  <assign|render-bib-list|<\macro|body>
    <\indent-left|<value|bibitem-width>>
      <surround|<no-page-break*>|<no-indent*>|<arg|body>>
    </indent-left>
  </macro>>

  <active*|<\src-comment>
    Indexes and glossaries.
  </src-comment>>

  <assign|index-sep|<macro|, >>

  <assign|index-1|<macro|left|right|<margin-first-other|0em|2.3em|<arg|left><index-sep><arg|right>>>>

  <assign|index-1*|<macro|left|<margin-first-other|0em|2.3em|<arg|left><no-page-break>>>>

  <assign|index-2|<macro|left|right|<margin-first-other|1em|2.3em|<arg|left><index-sep><arg|right>>>>

  <assign|index-2*|<macro|left|<margin-first-other|1em|2.3em|<arg|left><no-page-break>>>>

  <assign|index-3|<macro|left|right|<margin-first-other|1.7em|2.3em|<arg|left><index-sep><arg|right>>>>

  <assign|index-3*|<macro|left|<margin-first-other|1.7em|2.3em|<arg|left><no-page-break>>>>

  <assign|index-4|<macro|left|right|<margin-first-other|2.3em|2.3em|<arg|left><index-sep><arg|right>>>>

  <assign|index-4*|<macro|left|<margin-first-other|2.3em|2.3em|<arg|left><no-page-break>>>>

  <assign|index-5|<macro|left|right|<margin-first-other|2.8em|2.8em|<arg|left><index-sep><arg|right>>>>

  <assign|index-5*|<macro|left|<margin-first-other|2.8em|2.8em|<arg|left><no-page-break>>>>

  \;

  <assign|glossary-sv|<macro|left|right|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hpart|1>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|2|2|cell-valign|b>|<cwith|1|1|2|2|cell-halign|r>|<cwith|1|1|2|2|cell-width|1.8em>|<cwith|1|-1|1|-1|cell-lsep|0cm>|<cwith|1|-1|1|-1|cell-rsep|0cm>|<cwith|1|1|1|1|cell-halign|L>|<table|<row|<\cell>
    <surround||<glossary-dots>|<with|par-mode|left|<arg|left>>>
  </cell>|<cell|<arg|right>>>>>>>>

  <assign|glossary-1|<macro|left|right|<glossary-sv|<arg|left>|<arg|right>>>>

  <assign|glossary-2|<\macro|entry|explain|right>
    <glossary-sv|<margin-first-other|0fn|10fn|<style-with|src-compact|none|<resize|<arg|entry>
    |||<maximum|1r|10fn>|><arg|explain>>>|<arg|right>>
  </macro>>

  \;

  <assign|render-index|<\macro|name|body>
    <\with|par-par-sep|-0.5fn>
      <principal-section*|<arg|name>>

      \;
    </with>

    <\small>
      <\with|par-first|0fn|par-par-sep|0fn|par-columns|2|par-columns-sep|1cc>
        <arg|body>
      </with>
    </small>
  </macro>>

  <assign|render-glossary|<\macro|name|body>
    <principal-section*|<arg|name>>

    <\small>
      <\with|par-first|0fn|par-par-sep|0fn>
        <arg|body>
      </with>
    </small>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|6>
  </collection>
</initial>