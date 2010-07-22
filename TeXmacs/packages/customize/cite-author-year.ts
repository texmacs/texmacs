<TeXmacs|1.0.7.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|cite-author-year|1.0>

    <\src-purpose>
      This package contains extended macros for citations and provides a
      <TeXmacs> counterpart for the natbib package.
    </src-purpose>

    <src-copyright|2006|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(text std-text-natbib)>

  <\active*>
    <\src-comment>
      Helper macros
    </src-comment>
  </active*>

  <assign|natbib-author|<macro|text|<extern|natbib-get|<arg|text>|author>>>

  <assign|natbib-year|<macro|text|<extern|natbib-get|<arg|text>|year>>>

  <assign|natbib-author*|<macro|text|<extern|natbib-get|<arg|text>|author*>>>

  \;

  <assign|cite-add|<macro|key|<write|bib|<arg|key>>>>

  <assign|cite-data|<macro|key|<get-binding|<merge|bib-|<arg|key>>>>>

  <assign|cite-link|<macro|key|text|<style-with|src-compact|none|<locus|<id|<hard-id|<arg|key>>>|<link|hyperlink|<id|<hard-id|<arg|key>>>|<url|<merge|#bib-|<arg|key>>>>|<arg|text>>>>>

  <\active*>
    <\src-comment>
      Main commands for citations
    </src-comment>
  </active*>

  <assign|render-cite|<macro|x|(<arg|x>)>>

  <assign|cite-sep|<macro|; >>

  \;

  <assign|cite-author|<macro|x|<cite-add|<arg|x>><natbib-author|<cite-data|<arg|x>>>>>

  <assign|cite-year|<macro|x|<cite-add|<arg|x>><natbib-year|<cite-data|<arg|x>>>>>

  <assign|cite-author*|<macro|x|<cite-add|<arg|x>><natbib-author*|<cite-data|<arg|x>>>>>

  <assign|cite-author-year|<macro|x|<cite-author|<arg|x>>,
  <cite-year|<arg|x>>>>

  <assign|cite-author*-year|<macro|x|<cite-author*|<arg|x>>,
  <cite-year|<arg|x>>>>

  <assign|cite-author-year*|<macro|x|<cite-author|<arg|x>>
  <render-cite|<cite-year|<arg|x>>>>>

  <assign|cite-author*-year*|<macro|x|<cite-author*|<arg|x>>
  <render-cite|<cite-year|<arg|x>>>>>

  \;

  <assign|cite-author-link|<macro|x|<cite-link|<arg|x>|<cite-author|<arg|x>>>>>

  <assign|cite-year-link|<macro|x|<cite-link|<arg|x>|<cite-year|<arg|x>>>>>

  <assign|cite-author*-link|<macro|x|<cite-link|<arg|x>|<cite-author*|<arg|x>>>>>

  \;

  <assign|cite-raw-1|<macro|x|<cite-link|<arg|x>|<cite-author-year|<arg|x>>>>>

  <assign|cite-raw+|<macro|x|<cite-sep><cite-raw-1|<arg|x>>>>

  <assign|cite-raw|<xmacro|x|<cite-raw-1|<arg|x|0>><map-args|cite-raw+|concat|x|1>>>

  <assign|cite-raw*-1|<macro|x|<cite-link|<arg|x>|<cite-author*-year|<arg|x>>>>>

  <assign|cite-raw*+|<macro|x|<cite-sep><cite-raw*-1|<arg|x>>>>

  <assign|cite-raw*|<xmacro|x|<cite-raw*-1|<arg|x|0>><map-args|cite-raw*+|concat|x|1>>>

  \;

  <assign|cite-textual-1|<macro|x|<cite-link|<arg|x>|<cite-author-year*|<arg|x>>>>>

  <assign|cite-textual+|<macro|x|<cite-sep><cite-textual-1|<arg|x>>>>

  <assign|cite-textual|<xmacro|x|<cite-textual-1|<arg|x|0>><map-args|cite-textual+|concat|x|1>>>

  <assign|cite-textual*-1|<macro|x|<cite-link|<arg|x>|<cite-author*-year*|<arg|x>>>>>

  <assign|cite-textual*+|<macro|x|<cite-sep><cite-textual*-1|<arg|x>>>>

  <assign|cite-textual*|<xmacro|x|<cite-textual*-1|<arg|x|0>><map-args|cite-textual*+|concat|x|1>>>

  \;

  <assign|cite|<xmacro|x|<render-cite|<cite-raw-1|<arg|x|0>><map-args|cite-raw+|concat|x|1>>>>

  <assign|cite*|<xmacro|x|<render-cite|<cite-raw-1|<arg|x|0>><map-args|cite-raw*+|concat|x|1>>>>

  <assign|cite-parenthesized|<value|cite>>

  <assign|cite-parenthesized*|<value|cite*>>

  <assign|cite-detail|<macro|x|y|<render-cite-detail|<cite-raw-1|<arg|x>>|<arg|y>>>>

  <assign|bibitem*|<macro|text|<style-with|src-compact|none|<render-bibitem|<natbib-author*|<arg|text>>,
  <natbib-year|<arg|text>>><set-binding|<arg|text>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>