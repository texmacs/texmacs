<TeXmacs|1.0.4.6>

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
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you do not have a copy of the license, then write to
        the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
        Boston, MA 02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env-base|env-math|env-float|header-article|title-base|section-article|std-latex>

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

  <assign|tiny|<macro|x|<with|font-base-size|5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|7|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-base-size|9|par-sep|2pt|<arg|x>>>>

  <assign|small|<macro|x|<style-with|src-compact|none|<with|font-base-size|9|par-sep|2pt|tex-above-display-skip|<macro|<tex-len|8.5pt|3pt|4pt>>|tex-below-display-skip|<macro|<tex-len|8.5pt|3pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0pt|2pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|4pt|2pt|2pt>>|<arg|x>>>>>

  <assign|normal-size|<macro|x|<style-with|src-compact|none|<with|font-base-size|10|par-sep|0.2em|tex-above-display-skip|<macro|<tex-len|3mm|6pt|4pt>>|tex-below-display-skip|<macro|<tex-len|3mm|6pt|4pt>>|tex-above-display-short-skip|<macro|<tex-len|0mm|6pt|0pt>>|tex-below-display-short-skip|<macro|<tex-len|2mm|4pt|4pt>>|<arg|x>>>>>

  <assign|large|<macro|x|<with|font-base-size|12|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|14|par-sep|<minus|<tmlen|16dd>|<tmlen|14pt>>|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|17|par-sep|<minus|<tmlen|17dd>|<tmlen|17pt>>|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|20|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-base-size|25|par-sep|5pt|<arg|x>>>>

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

  <active*|<\src-comment>
    Theorem-like environemments rendering.
  </src-comment>>

  <group-common-counter|theorem-env>

  <assign|theorem-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|theorem-sep|<macro|. >>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>

  <assign|remark-sep|<macro|. >>

  <assign|exercise-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|exercise-sep|<macro|. >>

  \;

  <assign|render-remark|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<remark-name|<arg|which><theorem-sep>>||<arg|body>>>
  </macro>>

  <assign|render-theorem|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<theorem-name|<arg|which><theorem-sep>>||<with|font-shape|italic|<arg|body>>>>
  </macro>>

  <assign|render-exercise|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<exercise-name|<arg|which><theorem-sep>>||<arg|body>>>
  </macro>>

  \;

  <assign|proof-text|<macro|<localize|Proof>>>

  <assign|dueto|<macro|name|<with|font-shape|right|<theorem-name|(<arg|name>)
  >>>>

  <assign|render-proof|<\macro|which|body>
    <\surround||<space|0.5fn><active*|<with|mode|math|\<box\>>>>
      <render-remark|<arg|which>|<arg|body>>
    </surround>
  </macro>>

  <assign|proof|<\macro|body>
    <render-proof|<proof-text>|<arg|body>>
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
      Title information.
    </src-comment>
  </active*>

  <assign|doc-title-block|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
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

  <assign|doc-abstract|<\macro|body>
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

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-odd-header|<small|<style-with|src-compact|none|<no-indent><arg|name><htab|5mm><quote|<page-the-page>>>>>>>>

  <assign|header-author|<macro|name|<assign|page-even-header|<small|<style-with|src-compact|none|<no-indent><quote|<page-the-page>><htab|5mm><arg|name>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      List environments.
    </src-comment>
  </active*>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<with|par-first|-15pt|<yes-indent>><resize|<arg|x>
  |r-15pt||r+0pt|>>>>

  <assign|compact-item|<macro|x|<style-with|src-compact|none|<with|par-first|-15pt|<yes-indent>><resize|<arg|x>|||r]15pt|>>>>

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

  <new-list|itemize-1|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<group|->>>>>

  <new-list|itemize-2|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<group|->>>>>

  <new-list|itemize-3|<value|aligned-item>|<macro|x|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-minus|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<group|->>>>>

  <new-list|itemize-dot|<value|aligned-item>|<macro|x|<active*|<with|mode|math|\<bullet\>>>>>

  <new-list|itemize-arrow|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<group|\<rightarrow\>>>>>>

  \;

  <assign|aligned-accolade-item|<macro|x|<aligned-item|<with|font-shape|right|{<arg|x>}>>>>

  <new-list|enumerate-1|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-2|<value|aligned-accolade-item>|<macro|x|<number|<arg|x>|alpha>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>

  \;

  <assign|compact-dash-item|<macro|x|<compact-item|<arg|x> <emdash> >>>

  <assign|long-compact-space-item|<macro|x|<item-long|<compact-space-item|<arg|x>>>>>

  <new-list|description-compact|<value|compact-space-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<group|\<ast\>>>>>>>

  <new-list|description-aligned|<value|aligned-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<group|\<ast\>>>>>>>

  <new-list|description-dash|<value|compact-dash-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<group|\<ast\>>>>>>>

  <new-list|description-long|<value|long-compact-space-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<group|\<ast\>>>>>>>

  <new-list|description|<value|compact-space-item>|<macro|x|<active*|<with|mode|math|<with|math-font-series|bold|<group|\<ast\>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|6>
  </collection>
</initial>