<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-framed|1.0>

    <\src-purpose>
      Highlighted fragments for <TeXmacs> documentation.
    </src-purpose>

    <src-copyright|2001--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|framed-program>

  <\active*>
    <\src-comment>
      Additional style parameters.
    </src-comment>
  </active*>

  \;

  <assign|intro-color|pastel yellow>

  <assign|body-color|pastel blue>

  <assign|frame-color|dark grey>

  <\active*>
    <\src-comment>
      Framed fragments.
    </src-comment>
  </active*>

  <assign|framed-table|<macro|body|<with|color|<value|frame-color>|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-background|<value|body-color>>|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<arg|body>>>>>

  <assign|framed-fragment|<macro|body|<surround||<no-indent*>|<framed-table|<tformat|<table|<row|<\cell>
    <with|color|black|<arg|body>>
  </cell>>>>>>>>

  <assign|framed-fragment*|<\macro|body>
    <framed-table|<tformat|<twith|table-width|0.45par>|<table|<row|<\cell>
      <with|color|black|<arg|body>>
    </cell>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of <TeXmacs> code.
    </src-comment>
  </active*>

  <assign|tm-fragment|<\macro|body>
    <pseudo-code|<arg|body>>
  </macro>>

  <\active*>
    <\src-comment>
      Fragments of scheme code.
    </src-comment>
  </active*>

  <assign|scm-verb|<macro|body|<with|prog-language|verbatim|<arg|body>>>>

  <assign|scm-arg|<macro|body|<with|prog-font-shape|italic|<scm|<scm-verb|<arg|body>>>>>>

  <assign|scm-args|<macro|body|<with|prog-font-shape|italic|<scm|<scm-verb|<arg|body>>>><rsup|*>>>

  <assign|scm-opt-arg|<macro|body|<with|color|dark
  grey|[<style-with|<scm-arg|<arg|body>>>]>>>

  <\active*>
    <\src-comment>
      Fragments of mathemagix code.
    </src-comment>
  </active*>

  <assign|mmxlib|<macro|<with|font-shape|small-caps|Mmxlib>>>

  <assign|mmx-fragment*|<\macro|body>
    <framed-fragment|<with|par-par-sep|0fn|<mmx|<arg|body>>>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|par-par-sep|0fn>
    <associate|preamble|true>
    <associate|sfactor|3>
  </collection>
</initial>