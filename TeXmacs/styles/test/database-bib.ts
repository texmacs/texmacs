<TeXmacs|1.99.2>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|database-bib|1.0>

      <\src-purpose>
        Style for editing bibliographic databases.
      </src-purpose>

      <\src-copyright|2015>
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

  <use-package|database>

  <assign|db-kind|bib>

  <\active*>
    <\src-comment>
      Markup for bibliography fields.
    </src-comment>
  </active*>

  <assign|name-sep|<macro|<with|color|dark green|font-series|bold|
  <localize|and> >>>

  <assign|name-von|<macro|von|<with|font-shape|small-caps|color|dark
  grey|<arg|von>>>>

  <assign|name-jr|<macro|jr|<with|font-shape|small-caps|color|dark
  grey|<arg|jr>>>>

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
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>