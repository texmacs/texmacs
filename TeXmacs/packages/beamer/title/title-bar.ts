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

  <\active*>
    <\src-comment>
      External scheme routines
    </src-comment>
  </active*>

  <assign|screens-index|<macro|body|<extern|screens-index|<quote-arg|body>>>>

  <assign|screens-arity|<macro|body|<extern|screens-arity|<quote-arg|body>>>>

  <assign|screens-summary|<macro|body|<if|<greater|<screens-arity|<quote-arg|body>>|0>|<move|<tiny|<plus|<screens-index|<quote-arg|body>>|1>/<screens-arity|<quote-arg|body>>>|0em|0.25ex>>>>

  <assign|screens-bar|<macro|body|<extern|screens-bar|<quote-arg|body>>>>

  <\active*>
    <\src-comment>
      Customized title
    </src-comment>
  </active*>

  <assign|tit|<\macro|body>
    <\with|par-left|<minus|<value|page-screen-left>>|par-right|<minus|<value|page-screen-right>>>
      <shift|<with|color|<value|title-color>|math-color|<value|title-color>|<resize|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|-1|cell-background|<value|title-bar-color>>|<cwith|1|1|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-vcorrect|n>|<cwith|2|2|1|1|cell-halign|l>|<twith|table-valign|T>|<table|<row|<\cell>
        <phantom|<screens-summary|<quote-arg|body>>><htab|5mm><arg|body><htab|5mm><screens-summary|<quote-arg|body>>
      </cell>>|<row|<cell|<with|locus-color|grey|opacity|100%|<tiny|<screens-bar|<quote-arg|body>>>>>>>>>||0em||>>|0mm|<value|page-screen-top>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>