<TeXmacs|1.0.4.1>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|svjour|1.0>

      <\src-purpose>
        The SVJour style.
      </src-purpose>

      <\src-copyright|2002--2004>
        Adrien Bourdet
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

  <inactive*|<use-package|std|env-base|env-math|env-float|header-article|title-base|section-article|std-latex>>

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

  <assign|par-first|0fn>

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

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<style-with|src-compact|none|<vspace*|<plus|21dd|<tmlen|-4pt|0pt|4pt>>><normal-size|<arg|name>><vspace|<plus|10.5dd|<tmlen|-4pt|0pt|4pt>>>>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<style-with|src-compact|none|<vspace*|<plus|21dd|<tmlen|-4pt|0pt|4pt>>><normal-size|<arg|name>><vspace|<plus|10.5dd|<tmlen|-4pt|0pt|4pt>>>>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<plus|13dd|<tmlen|-4pt|0pt|4pt>>><normal-size|<arg|name>><space|5.5pt>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<vspace*|<plus|13dd|<tmlen|-4pt|0pt|4pt>>><normal-size|<arg|name>><space|5.5pt>>>>>

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

  <group-individual-counters|theorem-env>

  <assign|theorem-sep|<macro| >>

  \;

  <assign|new-theorem-bold|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem-bold>>>

  <assign|new-theorem-italic|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem-italic>>>

  <assign|new-theorem-bold-italic|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem-bold-italic>>>

  \;

  <assign|render-theorem-generic|<macro|which|body|<padded-normal|1fn|1fn|<\surround|<arg|><arg|which><arg|><theorem-sep>|>
    <\with|font-shape|italic>
      <arg|body>
    </with>
  </surround>>>>

  <assign|render-theorem-bold|<\macro|which|body>
    <\render-theorem-generic|<with|font-series|bold|<arg|which>>>
      <arg|body>
    </render-theorem-generic>
  </macro>>

  <assign|render-theorem-italic|<\macro|which|body>
    <\render-theorem-generic|<with|font-shape|italic|<arg|which>>>
      <arg|body>
    </render-theorem-generic>
  </macro>>

  <assign|render-theorem-bold-italic|<\macro|which|body>
    <\render-theorem-generic|<with|font-shape|italic|<with|font-series|bold|<arg|which>>>>
      <arg|body>
    </render-theorem-generic>
  </macro>>

  <active*|<\src-comment>
    Theorem-like environments.
  </src-comment>>

  <new-theorem-bold|theorem|Theorem>

  \;

  <new-theorem-italic|math-case|Case>

  <new-theorem-italic|conjecture|Conjecture>

  <new-theorem-italic|example|Example>

  <new-theorem-italic|note|Note>

  <new-theorem-italic|property|Property>

  <new-theorem-italic|question|Question>

  <new-theorem-italic|remark|Remark>

  \;

  <new-theorem-bold-italic|corollary|Corollary>

  <new-theorem-bold-italic|definition|Definition>

  <new-theorem-bold-italic|exercise|Exercise>

  <new-theorem-bold-italic|lemma|Lemma>

  <new-theorem-bold-italic|problem|Problem>

  <new-theorem-bold-italic|proposition|Proposition>

  <new-theorem-bold-italic|solution|Solution>

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
      The standard itemize environment with three levels.
    </src-comment>
  </active*>

  <new-list|itemize-1|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<group|->>>>>

  <new-list|itemize-2|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<group|->>>>>

  <new-list|itemize-3|<value|aligned-space-item>|<macro|x|<active*|<with|mode|math|<group|\<bullet\>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|6>
  </collection>
</initial>