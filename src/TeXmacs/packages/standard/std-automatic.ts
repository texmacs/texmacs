<TeXmacs|1.0.3.4>

<style|source>

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
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Citations.
    </src-comment>
  </active*>

  <assign|cite-arg|<macro|x|<write|bib|<arg|x>><reference|<merge|bib-|<arg|x>>>>>

  <assign|cite-arg-extra|<macro|x|, <cite-arg|<arg|x>>>>

  <assign|cite|<xmacro|x|[<cite-arg|<arg|x|0>><map-args|cite-arg-extra|concat|x|1>]>>

  <assign|cite-detail|<macro|x|y|[<cite-arg|<arg|x>>, <arg|y>]>>

  <assign|nocite-arg|<macro|x|<write|bib|<arg|x>>>>

  <assign|nocite|<xmacro|x|<style-with|src-compact|none|<flag|<localize|bibliography>|dark
  green|x><map-args|nocite-arg|concat|x|1>>>>

  <assign|bibitem|<macro|text|<style-with|src-compact|none|<item*|[<arg|text>]><assign|the-label|<arg|text>><label|<merge|bib-|<arg|text>>>>>>

  <assign|bibitem*|<macro|text|<style-with|src-compact|none|<item*|[<arg|text>]><assign|the-label|<arg|text>>>>>

  <assign|protect|>

  <assign|newblock|>

  <assign|citeauthoryear|<macro|author|year|<arg|author> <arg|year>>>

  <\active*>
    <\src-comment>
      Tables of contents.
    </src-comment>
  </active*>

  <new-counter|toc>

  <assign|the-toc|<macro|<merge|toc-|<value|toc-nr>>>>

  <assign|toc-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|toc-main-1|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<vspace*|2fn><with|font-series|bold|math-font-series|bold|font-size|1.19|<arg|what>><quote|<value|toc-dots>><pageref|<the-toc>><vspace|1fn>>>>>

  <assign|toc-main-2|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<vspace*|1fn><with|font-series|bold|math-font-series|bold|<arg|what>><quote|<value|toc-dots>><pageref|<the-toc>><vspace|0.5fn>>>>>

  <assign|toc-normal-1|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<arg|what><quote|<value|toc-dots>><pageref|<the-toc>>>>>>

  <assign|toc-normal-2|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<with|par-left|1.5fn|<arg|what><quote|<value|toc-dots>><pageref|<the-toc>>>>>>>

  <assign|toc-normal-3|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<with|par-left|3fn|<arg|what><quote|<value|toc-dots>><pageref|<the-toc>>>>>>>

  <assign|toc-small-1|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<with|par-left|6fn|font-size|0.84|<arg|what><quote|<value|toc-dots>><pageref|<the-toc>>>>>>>

  <assign|toc-small-2|<macro|what|<style-with|src-compact|none|<flag|<localize|table
  of contents>|dark green|what><inc-toc><label|<the-toc>><write|toc|<with|par-left|7.5fn|font-size|0.84|<arg|what><quote|<value|toc-dots>><pageref|<the-toc>>>>>>>

  <\active*>
    <\src-comment>
      Indexes.
    </src-comment>
  </active*>

  <new-counter|idx>

  <assign|the-idx|<macro|<merge|idx-|<value|idx-nr>>>>

  <assign|index-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|index-line|<macro|key|entry|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><write|idx|<tuple|<arg|key>||<arg|entry>>>>>>

  <assign|index-write|<macro|entry|<style-with|src-compact|none|<inc-idx><label|<the-idx>><write|idx|<tuple|<arg|entry>|<pageref|<the-idx>>>>>>>

  <assign|index|<macro|x|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><index-write|<tuple|<arg|x>>>>>>

  <assign|subindex|<macro|x|y|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><index-write|<tuple|<arg|x>|<arg|y>>>>>>

  <assign|subsubindex|<macro|x|y|z|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|x><index-write|<tuple|<arg|x>|<arg|y>|<arg|z>>>>>>

  <assign|index-complex|<macro|key|how|range|entry|<style-with|src-compact|none|<flag|<localize|index>|dark
  green|key><inc-idx><label|<the-idx>><write|idx|<tuple|<arg|key>|<arg|how>|<arg|range>|<arg|entry>|<pageref|<the-idx>>>>>>>

  <assign|index-1|<macro|left|right|<arg|left><value|index-dots><arg|right>>>

  <assign|index-1*|<macro|left|<arg|left><no-page-break>>>

  <assign|index-2|<macro|left|right|<with|par-left|1.5fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-2*|<macro|left|<with|par-left|1.5fn|<arg|left><no-page-break>>>>

  <assign|index-3|<macro|left|right|<with|par-left|3fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-3*|<macro|left|<with|par-left|3fn|<arg|left><no-page-break>>>>

  <assign|index-4|<macro|left|right|<with|par-left|4.5fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-4*|<macro|left|<with|par-left|4.5fn|<arg|left><no-page-break>>>>

  <assign|index-5|<macro|left|right|<with|par-left|6fn|<arg|left><value|index-dots><arg|right>>>>

  <assign|index-5*|<macro|left|<with|par-left|6fn|<arg|left><no-page-break>>>>

  <\active*>
    <\src-comment>
      Glossaries.
    </src-comment>
  </active*>

  <new-counter|gly>

  <assign|the-gly|<macro|<merge|gly-|<value|gly-nr>>>>

  <assign|glossary-dots| <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
  >

  <assign|glossary-line|<macro|entry|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><write|gly|<tuple|<arg|entry>>>>>>

  <assign|glossary|<macro|entry|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><inc-gly><label|<the-gly>><write|gly|<tuple|normal|<arg|entry>|<pageref|<the-gly>>>>>>>

  <assign|glossary-explain|<macro|entry|explain|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><inc-gly><label|<the-gly>><write|gly|<tuple|normal|<arg|entry>|<arg|explain>|<pageref|<the-gly>>>>>>>

  <assign|glossary-dup|<macro|entry|<style-with|src-compact|none|<flag|<localize|glossary>|dark
  green|entry><inc-gly><label|<the-gly>><write|gly|<tuple|dup|<arg|entry>|<pageref|<the-gly>>>>>>>

  <assign|glossary-1|<macro|left|right|<arg|left><value|glossary-dots><arg|right>>>

  <assign|glossary-2|<macro|entry|explain|right|<resize|<arg|entry>
  |||r]10fn|><arg|explain><value|glossary-dots><arg|right>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>