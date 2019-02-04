<TeXmacs|1.99.8>

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

  <use-module|(database db-markup)>

  <\active*>
    <\src-comment>
      Environment variables
    </src-comment>
  </active*>

  <assign|par-par-sep|0.25fn>

  <assign|par-first|0fn>

  <assign|padded-par-par-sep|0.25fn>

  <assign|indent-par-first|1.5fn>

  <\active*>
    <\src-comment>
      Database resources.
    </src-comment>
  </active*>

  <assign|db-entry-env|<\macro|cfg|cbg|cname|type|name|body>
    <\padded-normal|0.25em|0.75em>
      <\wide-std-framed-colored|<arg|cfg>|<arg|cbg>>
        <with|font-series|bold|<action|<with|color|<arg|cfg>|<copy|<change-case|<arg|type>|Upcase>>:>|mouse-fold|<arg|name>>
        <with|color|<arg|cname>|<arg|name>>>
      </wide-std-framed-colored>

      <\with|par-par-sep|0fn|par-flexibility|2.0>
        <arg|body>
      </with>
    </padded-normal>
  </macro>>

  <assign|db-entry|<\macro|rid|type|name|meta|body>
    <\db-entry-env|dark blue|pastel blue|dark green|<arg|type>|<arg|name>>
      <arg|body>
    </db-entry-env>
  </macro>>

  <drd-props|db-entry|arity|5|unaccessible|0|unaccessible|1|accessible|2|unaccessible|3|accessible|4>

  <\active*>
    <\src-comment>
      Folded presentation.
    </src-comment>
  </active*>

  <assign|db-folded-entry-env|<\macro|cfg|cbg|cname|type|name|body>
    <\padded-normal|0.25em|0.25em>
      <\wide-std-framed-colored|<arg|cfg>|<arg|cbg>>
        <with|font-series|bold|<action|<with|color|<arg|cfg>|<copy|<change-case|<arg|type>|Upcase>>:>|mouse-unfold|<arg|name>>
        <with|color|<arg|cname>|<arg|name>>>
      </wide-std-framed-colored>
    </padded-normal>
  </macro>>

  <assign|db-folded-entry|<\macro|rid|type|name|meta|body>
    <\db-folded-entry-env|dark blue|pastel blue|dark
    green|<arg|type>|<arg|name>>
      <arg|body>
    </db-folded-entry-env>
  </macro>>

  <drd-props|db-folded-entry|arity|5|unaccessible|0|unaccessible|1|accessible|2|unaccessible|3|unaccessible|4>

  <\active*>
    <\src-comment>
      Pretty presentation.
    </src-comment>
  </active*>

  <assign|db-kind|unknown>

  <assign|db-pretty-entry|<\macro|rid|type|name|meta|body>
    <extern|ext-db-pretty-entry|<value|db-kind>|<quote-arg|rid>|<quote-arg|type>|<quote-arg|name>|<quote-arg|meta>|<quote-arg|body>>
  </macro>>

  <drd-props|db-pretty-entry|arity|5|unaccessible|0|unaccessible|1|accessible|2|unaccessible|3|unaccessible|4>

  <assign|db-pretty|<macro|key|val|<surround||<right-flush>|<with|par-left|1tab|par-first|-1tab|par-flexibility|2.0|<action|<with|color|dark
  blue|<strong|<arg|key>. >>|db-pretty-notify|<arg|key>><arg|val>>>>>

  <assign|db-result|<macro|key|val|<surround||<right-flush>|<with|par-left|1tab|par-first|-1tab|par-flexibility|2.0|<action|<with|color|dark
  blue|<strong|<arg|key>. >>|db-confirm-result|<arg|key>><arg|val>>>>>

  <\active*>
    <\src-comment>
      Individual fields for a resource.
    </src-comment>
  </active*>

  <assign|db-field-env|<\macro|ctype|cval|type|val>
    <\surround||<right-flush>>
      <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|min>|<cwith|1|-1|2|2|cell-halign|l>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|1|1|cell-width|10em>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|1|1|cell-lsep|1.5em>|<cwith|1|-1|2|2|cell-rsep|0em>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|<with|color|<arg|ctype>|<strong|<copy|<change-case|<arg|type>|Upcase>>>>>|<\cell>
        <with|color|<arg|cval>|<arg|val>>
      </cell>>>>>
    </surround>
  </macro>>

  <assign|db-field|<\macro|type|val>
    <db-field-env|dark blue|black|<arg|type>|<arg|val>>
  </macro>>

  <assign|db-field-optional|<\macro|type|val>
    <db-field-env|#6060c0|#808080|<arg|type>|<arg|val>>
  </macro>>

  <assign|db-field-alternative|<\macro|type|val>
    <db-field-env|dark green|black|<arg|type>|<arg|val>>
  </macro>>

  <drd-props|db-field|arity|2|unaccessible|0|accessible|1>

  <drd-props|db-field-optional|arity|2|unaccessible|0|accessible|1>

  <drd-props|db-field-alternative|arity|2|unaccessible|0|accessible|1>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>