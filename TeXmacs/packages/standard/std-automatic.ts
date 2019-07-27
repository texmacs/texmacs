<TeXmacs|1.99.11>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-automatic|1.0>

    <\src-purpose>
      This package contains macros for the automatic generation of content
      (citations, tables of contents, indexes and glossaries) and the
      rendering of such content.
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
      Automatically generated labels.
    </src-comment>
  </active*>

  <new-counter|auto>

  <assign|current-part|>

  <assign|set-part|<macro|Id|body|<with|current-part|<merge|<value|current-part>|.|<arg|Id>>|auto-nr|0|<arg|body>>>>

  <assign|the-auto|<macro|<merge|auto|<value|current-part>|-|<value|auto-nr>>>>

  <assign|auto-label|<macro|<inc-auto><label|<the-auto>>>>

  <\active*>
    <\src-comment>
      Citations.
    </src-comment>
  </active*>

  <assign|bib-prefix|bib>

  <assign|with-bib|<macro|bib|body|<with|bib-prefix|<arg|bib>|<arg|body>>>>

  <assign|render-cite|<macro|body|[<arg|body>]>>

  <assign|render-cite-detail|<macro|body|detail|<render-cite|<arg|body>,
  <arg|detail>>>>

  <assign|cite-sep|<macro|, >>

  <assign|cite-arg|<macro|key|<write|<value|bib-prefix>|<arg|key>><reference|<merge|<value|bib-prefix>|-|<arg|key>>>>>

  <assign|cite-arg-extra|<macro|key|<cite-sep><cite-arg|<arg|key>>>>

  <assign|cite|<xmacro|keys|<render-cite|<cite-arg|<arg|keys|0>><map-args|cite-arg-extra|concat|keys|1>>>>

  <assign|cite-detail|<macro|key|details|<render-cite-detail|<cite-arg|<arg|key>>|<arg|details>>>>

  <assign|cite-raw|<xmacro|keys|<cite-arg|<arg|keys|0>><map-args|cite-arg-extra|concat|keys|1>>>

  <assign|nocite-arg|<macro|key|<write|<value|bib-prefix>|<arg|key>>>>

  <assign|nocite|<xmacro|keys|<style-with|src-compact|none|<flag|<localize|bibliography>|dark
  green|keys><map-args|nocite-arg|concat|keys>>>>

  <drd-props|cite|arity|<tuple|repeat|1|1>|accessible|none|identifier|all>

  <drd-props|nocite|arity|<tuple|repeat|1|1>|accessible|none|identifier|all>

  <drd-props|cite-detail|arity|2|unaccessible|0|identifier|0>

  <\active*>
    <\src-comment>
      Bibliographies.
    </src-comment>
  </active*>

  <assign|bibitem-width|3em>

  <assign|transform-bibitem|<macro|body|<strong|[<arg|body>] \ >>>

  <assign|render-bibitem|<macro|text|<style-with|src-compact|none|<with|par-first|<minus|1tmpt|<value|bibitem-width>>|<yes-indent>><resize|<arg|text>|||<maximum|1r|<value|bibitem-width>>|>>>>

  <assign|bibitem|<macro|text|<style-with|src-compact|none|<bibitem*|<arg|text>><label|<merge|<value|bib-prefix>|-|<arg|text>>>>>>

  <assign|bibitem-with-key|<macro|text|key|<style-with|src-compact|none|<bibitem*|<arg|text>><label|<merge|<value|bib-prefix>|-|<arg|key>>>>>>

  <assign|bibitem*|<macro|text|<style-with|src-compact|none|<render-bibitem|<transform-bibitem|<arg|text>>><set-binding|<arg|text>>>>>

  <assign|protect|>

  <assign|newblock|>

  <assign|citeauthoryear|<macro|author|year|<arg|author> <arg|year>>>

  <assign|bibitem-hsep|<macro|<value|bibitem-width>>>

  <assign|bib-list|<\macro|largest|body>
    <\with|bibitem-width|<minimum|<box-info|<transform-bibitem|<arg|largest>>.|w.>|3em>|item-hsep|<value|bibitem-hsep>|bibitem-nr|0|par-flexibility|2.0>
      <\description>
        <arg|body>
      </description>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Tables of contents.
    </src-comment>
  </active*>

  <assign|toc-prefix|toc>

  <assign|with-toc|<macro|toc|body|<with|toc-prefix|<arg|toc>|<arg|body>>>>

  <assign|toc-entry|<macro|type|what|<quasi|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><auto-label><write|<value|toc-prefix>|<compound|<unquote|<arg|type>>|<arg|what>|<pageref|<the-auto>>>><toc-notify|<arg|type>|<arg|what>>>>>>

  <assign|toc-main-1|<macro|what|<toc-entry|toc-strong-1|<arg|what>>>>

  <assign|toc-main-2|<macro|what|<toc-entry|toc-strong-2|<arg|what>>>>

  <assign|toc-normal-1|<macro|what|<toc-entry|toc-1|<arg|what>>>>

  <assign|toc-normal-2|<macro|what|<toc-entry|toc-2|<arg|what>>>>

  <assign|toc-normal-3|<macro|what|<toc-entry|toc-3|<arg|what>>>>

  <assign|toc-small-1|<macro|what|<toc-entry|toc-4|<arg|what>>>>

  <assign|toc-small-2|<macro|what|<toc-entry|toc-5|<arg|what>>>>

  \;

  <assign|toc-dots|<macro| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >>

  <assign|toc-strong-1|<macro|left|right|<vspace*|2fn><with|font-series|bold|math-font-series|bold|font-size|1.19|<arg|left>><toc-dots><no-break><arg|right><vspace|1fn>>>

  <assign|toc-strong-2|<macro|left|right|<vspace*|1fn><with|font-series|bold|math-font-series|bold|<arg|left>><toc-dots><no-break><arg|right><vspace|0.5fn>>>

  <assign|toc-1|<macro|left|right|<arg|left><toc-dots><no-break><arg|right>>>

  <assign|toc-2|<macro|left|right|<with|par-left|1tab|<arg|left><toc-dots><no-break><arg|right>>>>

  <assign|toc-3|<macro|left|right|<with|par-left|2tab|<arg|left><toc-dots><no-break><arg|right>>>>

  <assign|toc-4|<macro|left|right|<with|par-left|4tab|<arg|left><toc-dots><no-break><arg|right><vspace|0.15fn>>>>

  <assign|toc-5|<macro|left|right|<arg|left><toc-dots><no-break><arg|right><vspace|0.15fn>>>

  <\active*>
    <\src-comment>
      Indexes.
    </src-comment>
  </active*>

  <assign|index-enabled|true>

  <assign|index-prefix|idx>

  <assign|with-index|<macro|idx|body|<with|index-prefix|<arg|idx>|<arg|body>>>>

  <assign|index-line|<macro|key|entry|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><write|<value|index-prefix>|<tuple|<arg|key>||<arg|entry>>>>>>

  <assign|index-write|<macro|entry|<style-with|src-compact|none|<if|<value|index-enabled>|<auto-label><write|<value|index-prefix>|<tuple|<arg|entry>|<pageref|<the-auto>>>>>>>>

  <assign|index|<macro|key|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><index-write|<tuple|<arg|key>>>>>>

  <assign|subindex|<macro|key|secondary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><index-write|<tuple|<arg|key>|<arg|secondary>>>>>>

  <assign|subsubindex|<macro|key|secondary|tertiary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><index-write|<tuple|<arg|key>|<arg|secondary>|<arg|tertiary>>>>>>

  <assign|subsubsubindex|<macro|key|secondary|tertiary|quaternary|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><index-write|<tuple|<arg|key>|<arg|secondary>|<arg|tertiary>|<arg|quaternary>>>>>>

  <assign|index-complex|<macro|key|how|range|entry|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><auto-label><write|<value|index-prefix>|<tuple|<arg|key>|<arg|how>|<arg|range>|<arg|entry>|<pageref|<the-auto>>>>>>>

  \;

  <assign|index-dots|<macro| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >>

  <assign|index-1|<macro|left|right|<with|par-left|0tab|<arg|left><index-dots><arg|right>>>>

  <assign|index-1*|<macro|left|<with|par-left|0tab|<arg|left><no-page-break>>>>

  <assign|index-2|<macro|left|right|<with|par-left|1tab|<arg|left><index-dots><arg|right>>>>

  <assign|index-2*|<macro|left|<with|par-left|1tab|<arg|left><no-page-break>>>>

  <assign|index-3|<macro|left|right|<with|par-left|2tab|<arg|left><index-dots><arg|right>>>>

  <assign|index-3*|<macro|left|<with|par-left|2tab|<arg|left><no-page-break>>>>

  <assign|index-4|<macro|left|right|<with|par-left|3tab|<arg|left><index-dots><arg|right>>>>

  <assign|index-4*|<macro|left|<with|par-left|3tab|<arg|left><no-page-break>>>>

  <assign|index-5|<macro|left|right|<with|par-left|4tab|<arg|left><index-dots><arg|right>>>>

  <assign|index-5*|<macro|left|<with|par-left|4tab|<arg|left><no-page-break>>>>

  \;

  <assign|index-break-style|recall>

  <assign|index+1|<macro|l1|right|<index-1|<arg|l1>|<arg|right>>>>

  <assign|index+1*|<macro|l1|<index-1*|<arg|l1>>>>

  <assign|index+2|<macro|l1|l2|right|<index-2|<\if-page-break|t>
    <index-1*|<arg|l1>>
  </if-page-break><arg|l2>|<arg|right>>>>

  <assign|index+2*|<macro|l1|l2|<index-2*|<\if-page-break|t>
    <index-1*|<arg|l1>>
  </if-page-break><arg|l2>>>>

  <assign|index+3|<macro|l1|l2|l3|right|<index-3|<\if-page-break|t>
    <index-1*|<arg|l1>>

    <index-2*|<arg|l2>>
  </if-page-break><arg|l3>|<arg|right>>>>

  <assign|index+3*|<macro|l1|l2|l3|<index-3*|<\if-page-break|t>
    <index-1*|<arg|l1>>

    <index-2*|<arg|l2>>
  </if-page-break><arg|l3>>>>

  <assign|index+4|<macro|l1|l2|l3|l4|right|<index-4|<\if-page-break|t>
    <index-1*|<arg|l1>>

    <index-2*|<arg|l2>>

    <index-3*|<arg|l3>>
  </if-page-break><arg|l4>|<arg|right>>>>

  <assign|index+4*|<macro|l1|l2|l3|l4|<index-4*|<\if-page-break|t>
    <index-1*|<arg|l1>>

    <index-2*|<arg|l2>>

    <index-3*|<arg|l3>>
  </if-page-break><arg|l4>>>>

  <assign|index+5|<macro|l1|l2|l3|l4|l5|right|<index-5|<\if-page-break|t>
    <index-1*|<arg|l1>>

    <index-2*|<arg|l2>>

    <index-3*|<arg|l3>>

    <index-4*|<arg|l4>>
  </if-page-break><arg|l5>|<arg|right>>>>

  <assign|index+5*|<macro|l1|l2|l3|l4|l5|<index-5*|<\if-page-break|t>
    <index-1*|<arg|l1>>

    <index-2*|<arg|l2>>

    <index-3*|<arg|l3>>

    <index-4*|<arg|l4>>
  </if-page-break><arg|l5>>>>

  <\active*>
    <\src-comment>
      Glossaries.
    </src-comment>
  </active*>

  <assign|glossary-prefix|gly>

  <assign|with-glossary|<macro|gly|body|<with|glossary-prefix|<arg|gly>|<arg|body>>>>

  <assign|glossary-line|<macro|entry|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><write|<value|glossary-prefix>|<tuple|<arg|entry>>>>>>

  <assign|glossary|<macro|entry|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><auto-label><write|<value|glossary-prefix>|<tuple|normal|<arg|entry>|<pageref|<the-auto>>>>>>>

  <assign|glossary-explain|<macro|entry|explain|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><auto-label><write|<value|glossary-prefix>|<tuple|normal|<arg|entry>|<arg|explain>|<pageref|<the-auto>>>>>>>

  <assign|glossary-dup|<macro|entry|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><auto-label><write|<value|glossary-prefix>|<tuple|dup|<arg|entry>|<pageref|<the-auto>>>>>>>

  \;

  <assign|glossary-dots|<macro| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >>

  <assign|glossary-1|<macro|left|right|<arg|left><glossary-dots><no-break><arg|right>>>

  <assign|glossary-2|<macro|entry|explain|right|<margin-first-other|0fn|10fn|<style-with|src-compact|none|<resize|<arg|entry>
  |||<maximum|1r|10fn>|><arg|explain><glossary-dots><no-break><arg|right>>>>>

  <\active*>
    <\src-comment>
      Shared documents.
    </src-comment>
  </active*>

  <assign|part-info|<macro|name|<style-with|src-compact|none|<label|<merge|part:|<arg|name>>><write|parts|<tuple|<arg|name>|chapter-nr|<value|chapter-nr>|section-nr|<value|section-nr><value|>|subsection-nr|<value|subsection-nr>>>>>>

  <assign|shared|<macro|unique-id|mirror-id|body|<with|old-color|<value|locus-color>|locus-color|preserve|<locus|<id|<arg|mirror-id>>|<observer|<arg|unique-id>|mirror-notify>|<surround|<hidden|<part-info|<arg|unique-id>><extern|mirror-initialize|<quote-arg|xbody>>>|<right-flush>|<with|locus-color|<value|old-color>|<arg|body>>>>>>>

  <assign|include|<macro|name|<\surround|<part-info|<arg|name>>|>
    <include*|<arg|name>>
  </surround>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>