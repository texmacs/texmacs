<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|title-bar|1.0>

    <\src-purpose>
      A style package for title bars
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(dynamic fold-markup)>

  <assign|xpage-screen-top|2.5mm>

  <assign|screens-index|<macro|body|<extern|screens-index|<quote-arg|body>>>>

  <assign|screens-arity|<macro|body|<extern|screens-arity|<quote-arg|body>>>>

  <assign|screens-summary|<macro|body|<if|<greater|<screens-arity|<quote-arg|body>>|0>|<move|<tiny|<plus|<screens-index|<quote-arg|body>>|1>/<screens-arity|<quote-arg|body>>>|0em|0.25ex>>>>

  <assign|tit|<\macro|body>
    <\with|par-left|<minus|<value|page-screen-left>>|par-right|<minus|<value|page-screen-right>>>
      <shift|<with|color|<value|title-color>|math-color|<value|title-color>|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-background|<value|title-bar-color>>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|1|1|cell-halign|l>|<cwith|1|1|2|2|cell-halign|c>|<table|<row|<cell|<phantom|<screens-summary|<quote-arg|body>>>>|<cell|<arg|body>>|<cell|<screens-summary|<quote-arg|body>>>>>>>>|0mm|<value|page-screen-top>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>