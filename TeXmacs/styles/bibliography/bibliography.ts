<TeXmacs|1.0.7.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|bibliography|1.0>

      <\src-purpose>
        The bibliography style.
      </src-purpose>

      <\src-copyright|1998--2004>
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

  <use-package|std|env>

  \;

  <assign|color|red>

  <assign|par-first|0fn>

  <assign|par-par-sep|0.6666fn>

  <\active*>
    <\src-comment>
      Bibliography entries.
    </src-comment>
  </active*>

  <assign|bib-entry-env|<\macro|mfield|cfg|cbg|cid|pre|type|id|body>
    <\padded-normal|0.25em|1.5em>
      <\wide-std-framed-colored|<arg|cfg>|<arg|cbg>>
        <with|color|<arg|cfg>|<arg|pre>><with|font-series|bold|<with|color|<arg|cfg>|<arg|type>:
        ><with|color|<arg|cid>|<arg|id>>>
      </wide-std-framed-colored>

      <\with|bib-field|<arg|mfield>|par-par-sep|0fn>
        <arg|body>
      </with>
    </padded-normal>
  </macro>>

  <assign|bib-entry|<\macro|type|id|body>
    <\with|mfield|<macro|t|v|<bib-field-env|dark
    blue|black|<macro|b|<with|color|dark green|<strong|<arg|b>>>>||<arg|t>|<arg|v>>>>
      <\bib-entry-env|<value|mfield>|dark blue|pastel blue|dark
      green||<arg|type>|<arg|id>>
        <arg|body>
      </bib-entry-env>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Bibliography fields.
    </src-comment>
  </active*>

  <assign|bib-field-env|<\macro|ctype|cval|mvar|pre|type|val>
    <\surround||<right-flush>>
      <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|1|1|cell-width|10em>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|1|1|cell-lsep|1.5em>|<cwith|1|-1|2|2|cell-rsep|0em>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|<with|color|<arg|ctype>|<arg|pre><strong|<arg|type>>>>|<\cell>
        <with|color|<arg|cval>|bib-var|<arg|mvar>|<arg|val>>
      </cell>>>>>
    </surround>
  </macro>>

  <assign|bib-field|<\macro|type|val>
    <bib-field-env|dark red|red|<macro|b|<with|color|dark
    red|<strong|<arg|b>>>>||<arg|type>|<arg|val>>
  </macro>>

  <\active*>
    <\src-comment>
      Markup for bibliography fields.
    </src-comment>
  </active*>

  <assign|bib-name|<macro|f|v|l|j|<if|<unequal|<length|<arg|f>>|0>|<arg|f>
  ><if|<unequal|<length|<arg|v>>|0>|<arg|><with|color|dark
  grey|font-shape|small-caps|<arg|v>> ><with|font-shape|small-caps|<arg|l>><if|<unequal|<length|<arg|j>>|0>|,
  <arg|><with|font-shape|italic|<arg|j>> >>>

  <assign|bib-name-extra|<macro|x| <with|color|dark green|<strong|and>>
  <arg|x>>>

  <assign|bib-names|<xmacro|args|<style-with|src-compact|none|<arg|args|0><map-args|bib-name-extra|concat|args|1>>>>

  <assign|bib-pages-extra|<macro|x|<with|color|dark
  green|<space|0.5spc><strong|--><space|0.5spc>><arg|x>>>

  <assign|bib-pages|<xmacro|args|<style-with|src-compact|none|<arg|args|0><map-args|bib-pages-extra|concat|args|1>>>>

  <assign|keepcase|<macro|x|<underline|<arg|x>>>>

  <assign|first-last|<macro|x|<extern|ext-first-last|<arg|x>>>>

  <drd-props|first-last|arity|1|accessible|0>

  <\active*>
    <\src-comment>
      Markup for BibTeX compatibility
    </src-comment>
  </active*>

  <assign|bib-comm-prefix|<with|font-series|bold|% >>

  <assign|bib-var|<macro|body|<with|color|red|<strong|<arg|body>>>>>

  <assign|bib-latex|<macro|body|<bib-line-env|red||<arg|body>>>>

  <assign|bib-assign|<macro|type|val|<bib-field-env|dark
  red|red|<macro|b|<with|color|dark red|<strong|<arg|b>>>>||<arg|type>|<arg|val>>>>

  <assign|bib-line-env|<\macro|cbody|pre|body>
    <with|color|<arg|cbody>|<arg|pre><arg|body>>
  </macro>>

  <assign|bib-line|<macro|body|<bib-line-env|red||<arg|body>>>>

  <assign|bib-string|<\macro|body>
    <\padded-normal|0.25em|1.5em>
      <\wide-std-framed-colored|dark green|pastel green>
        <with|font-series|bold|color|dark green|Strings: >
      </wide-std-framed-colored>

      <with|bib-assign|<macro|t|v|<bib-field-env|dark
      green|black|<macro|b|<with|color|dark
      green|<strong|<arg|b>>>>||<arg|t>|<arg|v>>>|bib-comment|<macro|b|<with|bib-assign|<macro|t|v|<bib-field-env|dark
      grey|dark grey|<macro|bb|<with|color|dark
      grey|<strong|<arg|bb>>>>|<value|bib-comm-prefix>|<arg|t>|<arg|v>>>|<arg|b>>>|<arg|body>>
    </padded-normal>
  </macro>>

  <assign|bib-preamble|<\macro|body>
    <\padded-normal|0.25em|1.5em>
      <\wide-std-framed-colored|dark orange|pastel orange>
        <with|font-series|bold|color|dark orange|Preamble: >
      </wide-std-framed-colored>

      <with|bib-latex|<macro|b|<bib-line-env|black||<arg|b>>>|bib-comment|<macro|b|<with|bib-latex|<macro|bb|<bib-line-env|dark
      grey|<value|bib-comm-prefix>|<arg|bb>>>|<arg|b>>>|par-left|1.5em|par-first|0|<arg|body>>
    </padded-normal>
  </macro>>

  <assign|bib-comment|<\macro|body>
    <\with|bib-field|<macro|t|v|<bib-field-env|dark grey|dark
    grey|<macro|b|<with|color|dark grey|<strong|<arg|b>>>>|<value|bib-comm-prefix>|<arg|t>|<arg|v>>>|bib-line|<macro|b|<bib-line-env|dark
    grey|<value|bib-comm-prefix>|<arg|b>>>>
      <with|bib-entry|<macro|t|i|b|<bib-entry-env|<value|bib-field>|dark
      grey|pastel grey|dark grey|<value|bib-comm-prefix>|<arg|t>|<arg|i>|<arg|b>>>|par-first|0|<padded-normal|0.25em|1.5em|<arg|body>>>
    </with>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>