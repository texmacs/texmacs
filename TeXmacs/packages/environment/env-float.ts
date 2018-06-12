<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-float|1.0>

    <\src-purpose>
      Environments for floating content.
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
      Style parameters
    </src-comment>
  </active*>

  <assign|figure-width|1par>

  <assign|figure-left-padding|0spc>

  <assign|figure-right-padding|0spc>

  <assign|figure-caption-sep|0.5fn>

  <assign|caption-left-padding|1.5fn>

  <assign|caption-right-padding|1.5fn>

  \;

  <assign|figure-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|figure-sep|<macro|. >>

  <assign|footnote-sep|<macro|. >>

  \;

  <drd-props|figure-sep|macro-parameter|string>

  <drd-props|footnote-sep|macro-parameter|string>

  <\active*>
    <\src-comment>
      Detailed and summarized captions
    </src-comment>
  </active*>

  <assign|caption-detailed|<\macro|long|short>
    <arg|long>
  </macro>>

  <assign|caption-summarized|<\macro|long|short>
    <arg|short>
  </macro>>

  <drd-props|caption-detailed|arity|2|accessible|0|border|no>

  <drd-props|caption-summarized|arity|2|accessible|1|border|no>

  <assign|list-prefix|<macro|type|<if|<provides|<merge|<arg|type>|-list-prefix>>|<value|<merge|<arg|type>|-list-prefix>>|<arg|type>>>>

  <assign|list-caption|<macro|type|cap|<with|caption-detailed|<value|caption-summarized>|<style-with|src-compact|none|<auto-label><write|<list-prefix|<arg|type>>|<tuple|normal|<arg|cap>|<pageref|<the-auto>>>>>>>>

  <\active*>
    <\src-comment>
      Figure-like environments.
    </src-comment>
  </active*>

  <assign|render-small-figure|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<table|<row|<cell|<resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>>>|<row|<cell|>>|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <arg|cap>
    </surround>>
  </cell>>>>>>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|caption-right-padding>>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </cell>>>>>>
  </macro>>

  <assign|figure-list-prefix|figure>

  <assign|with-figure-list|<macro|lof|body|<with|figure-list-prefix|<arg|lof>|<arg|body>>>>

  <assign|table-list-prefix|table>

  <assign|with-table-list|<macro|lot|body|<with|table-list-prefix|<arg|lot>|<arg|body>>>>

  <new-figure|figure|Figure>

  <new-figure|table|Table>

  <\active*>
    <\src-comment>
      Footnotes.
    </src-comment>
  </active*>

  <assign|render-footnote*|<macro|sym|nr|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<with|par-mode|justify|par-left|0cm|par-right|0cm|font-shape|right|dummy|<value|page-fnote-sep>|dummy|<value|page-fnote-barlen>|<style-with|src-compact|none|<surround|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<merge|#footnr-|<arg|nr>>>>|<arg|sym>><footnote-sep>|<set-binding|<merge|footnote-|<arg|nr>>|<value|the-label>|body><right-flush>|<style-with|src-compact|none|<arg|body>>>>>>
  </float>>>>

  <assign|render-footnote|<macro|nr|body|<render-footnote*|<arg|nr>|<arg|nr>|<arg|body>>>>

  <assign|footnote|<macro|body|<style-with|src-compact|none|<next-footnote><render-footnote|<the-footnote>|<arg|body>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<with|font-shape|right|<reference|<merge|footnote-|<the-footnote>>>>>>>>

  <\active*>
    <\src-comment>
      Customized notes.
    </src-comment>
  </active*>

  <assign|custom-note-text|<\macro|sym|id|body>
    <\style-with|src-compact|none>
      <style-with|src-compact|none|||<\surround|<locus|<id|<merge|dest-abbr-|<arg|id>>>|<link|footnote-source|<id|<merge|dest-abbr-|<arg|id>>>|<id|<merge|source-|<arg|id>>>>|<arg|sym>><footnote-sep>|>
        <\with|locus-color|preserve|visited-color|preserve>
          <\locus|<id|<merge|dest-|<arg|id>>>>
            <style-with|src-compact|none|<arg|body>>
          </locus>
        </with>
      </surround>>
    </style-with>
  </macro>>

  <assign|custom-footnote-text|<macro|sym|id|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<\with|par-mode|justify|par-left|0cm|par-right|0cm>
      <\custom-note-text|<arg|sym>|<arg|id>>
        <surround||<right-flush>|<arg|body>>
      </custom-note-text>
    </with>>
  </float>>>>

  <assign|custom-note-ref|<macro|sym|sep|id|body|<style-with|src-compact|none|<with|locus-color|preserve|visited-color|preserve|<locus|<id|<merge|source-|<arg|id>>>|<arg|body>>><rsup|<locus|<id|<merge|source-abbr-|<arg|id>>>|<link|footnote-text|<id|<merge|source-abbr-|<arg|id>>>|<id|<merge|dest-|<arg|id>>>>|<arg|sep><arg|sym>>>>>>

  <\active*>
    <\src-comment>
      Wide variants.
    </src-comment>
  </active*>

  <assign|wide-footnote|<macro|body|<with|par-columns|1|<footnote|<arg|body>>>>>

  <assign|wide-float|<macro|type|pos|body|<flag|wide float|dark
  brown><with|par-columns|1|<float|<arg|type>|<arg|pos>|<arg|body>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>