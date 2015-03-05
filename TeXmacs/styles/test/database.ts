<TeXmacs|1.99.2>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|database|1.0>

      <\src-purpose>
        Style for editing databases.
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

  <use-package|std|env>

  \;

  <assign|par-first|0fn>

  <assign|par-par-sep|0.6666fn>

  <\active*>
    <\src-comment>
      Database resources.
    </src-comment>
  </active*>

  <assign|db-resource-env|<\macro|cfg|cbg|cname|type|name|body>
    <\padded-normal|0.25em|1.5em>
      <\wide-std-framed-colored|<arg|cfg>|<arg|cbg>>
        <with|font-series|bold|<with|color|<arg|cfg>|<copy|<change-case|<arg|type>|Upcase>>:
        ><with|color|<arg|cname>|<arg|name>>>
      </wide-std-framed-colored>

      <\with|par-par-sep|0fn>
        <arg|body>
      </with>
    </padded-normal>
  </macro>>

  <assign|db-resource|<\macro|rid|type|name|body>
    <\db-resource-env|dark blue|pastel blue|dark green|<arg|type>|<arg|name>>
      <arg|body>
    </db-resource-env>
  </macro>>

  <drd-props|db-resource|arity|4|unaccessible|0|unaccessible|1|accessible|2|accessible|3>

  <\active*>
    <\src-comment>
      Individual entries for a resource.
    </src-comment>
  </active*>

  <assign|db-entry-env|<\macro|ctype|cval|type|val>
    <\surround||<right-flush>>
      <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|1|1|cell-width|10em>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|1|1|cell-lsep|1.5em>|<cwith|1|-1|2|2|cell-rsep|0em>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|<with|color|<arg|ctype>|<strong|<copy|<change-case|<arg|type>|Upcase>>>>>|<\cell>
        <with|color|<arg|cval>|<arg|val>>
      </cell>>>>>
    </surround>
  </macro>>

  <assign|db-entry|<\macro|type|val>
    <db-entry-env|dark blue|black|<arg|type>|<arg|val>>
  </macro>>

  <assign|db-entry-optional|<\macro|type|val>
    <db-entry-env|#6060c0|#808080|<arg|type>|<arg|val>>
  </macro>>

  <assign|db-entry-alternative|<\macro|type|val>
    <db-entry-env|dark green|black|<arg|type>|<arg|val>>
  </macro>>

  <drd-props|db-entry|arity|2|unaccessible|0|accessible|1>

  <drd-props|db-entry-optional|arity|2|unaccessible|0|accessible|1>

  <drd-props|db-entry-alternative|arity|2|unaccessible|0|accessible|1>

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

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>