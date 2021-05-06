<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|svjour|1.0>

      <\src-purpose>
        The SVJour style.
      </src-purpose>

      <\src-copyright|2004--2005>
        Joris van der Hoeven and Adrien Bourdet
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env-base|env-math|env-enunciation|env-float|env-program|header-article|title-base|section-article|std-latex|html-font-size>

  <assign|env-theorem-dtd|1.0>

  <active*|<\src-comment>
    TeX-like style parameters.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|-30pt>>>

  <assign|tex-even-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|-30pt>>>

  <assign|tex-text-width|<macro|<if|<equal|<value|par-columns>|1>|25.5cc|17.8cm>>>

  \;

  <assign|tex-voffset|<macro|0pt>>

  <assign|tex-top-margin|<macro|-10pt>>

  <assign|tex-head-height|<macro|12pt>>

  <assign|tex-head-sep|<macro|16.74pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|<if|<equal|<value|par-columns>|1>|517.5dd|640dd>>>

  <assign|tex-foot-height-heuristic|<macro|1em>>

  <assign|tex-foot-skip|<macro|30pt>>

  \;

  <assign|tex-footnote-sep|<macro|8pt>>

  <assign|tex-footnote-tm-barlen|<macro|0.4par>>

  <assign|tex-column-sep|<macro|1.5cc>>

  <assign|tex-float-sep|<macro|<tmlen|10pt|12pt|14pt>>>

  <assign|tex-margin-par-width|<macro|48pt>>

  <assign|tex-margin-par-sep|<macro|10pt>>

  <active*|<\src-comment>
    Global layout.
  </src-comment>>

  <assign|font-base-size|10>

  <assign|par-sep|0.2em>

  <assign|par-first|15pt>

  \;

  <assign|tex-jot|<macro|2pt>>

  <assign|tex-above-display-skip|<macro|<tex-len|3mm|6pt|4pt>>>

  <assign|tex-below-display-skip|<macro|<tex-len|3mm|6pt|4pt>>>

  <active*|<src-short-comment|orig- for original value>>

  <assign|orig-tex-above-display-short-skip|<macro|<tex-len|2mm|6pt|0pt>>>

  <assign|tex-above-display-short-skip|<macro|<tex-len|1mm|6pt|0pt>>>

  <assign|tex-below-display-short-skip|<macro|<tex-len|2mm|4pt|4pt>>>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-size|0.5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|0.7|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-size|0.9|par-sep|2pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-size|0.9|par-sep|2pt|tex-above-display-skip|<macro|<tex-len|8.5pt|3pt|4pt>>|tex-below-display-skip|<macro|<tex-len|8.5pt|3pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|4pt|2pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-size|1.0|par-sep|0.2em|tex-above-display-skip|<macro|<tex-len|3mm|6pt|4pt>>|tex-below-display-skip|<macro|<tex-len|3mm|6pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0mm|6pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|2mm|4pt|4pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-size|1.2|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|1.4|par-sep|<minus|<tmlen|16dd>|<tmlen|14pt>>|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|1.7|par-sep|<minus|<tmlen|17dd>|<tmlen|17pt>>|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|2.0|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|2.5|par-sep|5pt|<arg|x>>>>

  <active*|<\src-comment>
    Sectional macros.
  </src-comment>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<tex-len|21dd|4pt|4pt>><normal-size|<arg|name>><vspace|<tex-len|10.5dd|4pt|4pt>>>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<style-with|src-compact|none|<vspace*|<tex-len|21dd|4pt|4pt>><normal-size|<arg|name>><vspace|<tex-len|10.5dd|4pt|4pt>>>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|13dd|4pt|4pt>><normal-size|<arg|name>><space|5.5pt>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<tex-len|13dd|4pt|4pt>><normal-size|<arg|name>><space|5.5pt>>>>>

  <active*|<\src-comment>
    Section and environment numbering.
  </src-comment>>

  <assign|sectional-sep|<macro|<space|0.3fn>>>

  <assign|part-sep|<macro|.<space|0.6fn>>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|true>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-odd-header|<small|<style-with|src-compact|none|<no-indent><arg|name><htab|5mm><page-number>>>>>>>

  <assign|header-author|<macro|name|<assign|page-even-header|<small|<style-with|src-compact|none|<no-indent><page-number><htab|5mm><arg|name>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <active*|<\src-comment>
    Theorem-like environemments rendering.
  </src-comment>>

  <group-common-counter|theorem-env>

  <assign|enunciation-sep|<macro|<space|1spc>>>

  <assign|env-number|<macro|num|<arg|num>>>

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

  <new-remark|acknowledgments|Acknowledgments>

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
      List environments.
    </src-comment>
  </active*>

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

  <assign|bib-list-nosvjour|<value|bib-list>>

  <assign|bib-list|<\macro|largest|body>
    <\bib-list-nosvjour|<arg|largest>>
      <\with|par-left|<plus|<value|par-left>|10pt>>
        <arg|body>
      </with>
    </bib-list-nosvjour>
  </macro>>

  \;

  <assign|item-1|<macro|<active*|<with|mode|math|<rigid|->>>>>

  <assign|item-2|<macro|<active*|<with|mode|math|<rigid|->>>>>

  <assign|item-3|<macro|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-1|<value|aligned-item>|<macro|x|<item-tag>>>

  <new-list|itemize-2|<value|aligned-item>|<macro|x|<item-tag>>>

  <new-list|itemize-3|<value|aligned-item>|<macro|x|<item-tag>>>

  <new-list|itemize-minus|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<rigid|->>>>>

  <new-list|itemize-dot|<value|aligned-item>|<macro|x|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-arrow|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<rigid|\<rightarrow\>>>>>>

  \;

  <assign|aligned-accolade-item|<macro|x|<aligned-item|<with|font-shape|right|{<arg|x>}>>>>

  <assign|enum-1|<macro|name|<arg|name>>>

  <assign|enum-2|<macro|name|<number|<arg|name>|alpha>>>

  <assign|enum-3|<macro|name|<number|<arg|name>|roman>>>

  <new-list|enumerate-1|<value|aligned-dot-item>|<macro|x|<enum-tag|<arg|x>>>>

  <new-list|enumerate-2|<value|aligned-accolade-item>|<macro|x|<enum-tag|<arg|x>>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<macro|x|<enum-tag|<arg|x>>>>

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
      Title information.
    </src-comment>
  </active*>

  <assign|doc-title-block|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|par-first|0fn|<arg|body>>
  </cell>>>>>>>

  <assign|doc-make-title|<macro|body|<surround||<vspace|22.47pt>|<doc-title-block|<arg|body>>>>>

  <assign|doc-render-title|<macro|x|<\surround||<vspace|11.24pt>>
    <doc-title-block|<larger|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-subtitle|<macro|x|<\surround||<vspace|11.24pt>>
    <doc-title-block|<large|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-date|<\macro|body>
    <doc-title-block|<arg|body>>
  </macro>>

  <assign|render-abstract|<\macro|body>
    <padded-normal|1fn|1.85fn|<surround|<sectional-short-bold|<abstract-text>><space|4mm>||<arg|body>>>
  </macro>>

  <\active*>
    <\src-comment>
      Author information.
    </src-comment>
  </active*>

  <assign|doc-author-block|<\macro|body>
    <style-with|src-compact|none|<space|0pt><tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-width|1par>|<table|<row|<\cell>
      <arg|body>
    </cell>>>>>>
  </macro>>

  <assign|author-by|<macro|body|<arg|body>>>

  <assign|author-render-name|<macro|x|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<with|font-series|bold|<arg|x>>>>>>

  <assign|doc-authors-data|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <unquote*|<quote-arg|data>>
      </quasi>
    </style-with>
  </xmacro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|6>
  </collection>
</initial>