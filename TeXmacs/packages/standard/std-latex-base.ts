<TeXmacs|1.99.13>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-latex-base|1.0>

    <\src-purpose>
      Macros which make it easier to port TeX/LaTeX style files to TeXmacs,
      base part
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <active*|<\src-comment>
    TeX-like lengths.
  </src-comment>>

  <assign|tex-len|<macro|default|increase|decrease|<style-with|src-compact|none|<tmlen|<minus|<arg|default>|<arg|decrease>>|<arg|default>|<plus|<arg|default>|<arg|increase>>>>>>

  <active*|<\src-comment>
    TeX style lists.
  </src-comment>>

  <assign|itemize-reduce|<macro|nr|<minimum|<arg|nr>|4>>>

  <assign|enumerate-reduce|<macro|nr|<minimum|<arg|nr>|4>>>

  <active*|<\src-comment>
    LaTeX boxes.
  </src-comment>>

  <assign|minipage|<macro|pos|width|body|<tabular|<tformat|<cwith|1|1|1|1|cell-width|<arg|width>>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<twith|table-valign|<arg|pos>>|<table|<row|<cell|<arg|body>>>>>>>>

  <assign|fcolorbox|<\macro|border|background|body>
    <with|text-color|<value|color>|<with|color|<arg|border>|<colored-frame|<arg|background>|<with|color|<value|text-color>|<arg|body>>>>>
  </macro>>

  <active*|<\src-comment>
    <LaTeX> preview
  </src-comment>>

  <assign|latex_preview|<macro|pic|src|<block|<tformat|<cwith|1|1|1|1|cell-background|dark
  grey>|<table|<row|<cell|<with|color|white|font-family|ss|<small|<LaTeX>
  preview>>>>|<row|<cell|<verbatim|<arg|src>>>>>>>>>

  <\active*>
    <\src-comment>
      Footnotes.
    </src-comment>
  </active*>

  <assign|footnotemark|<macro|<rsup|<with|font-shape|right|<next-footnote><reference|<merge|footnote-|<the-footnote>>>>>>>

  <assign|footnotemark*|<macro|num|<rsup|<with|font-shape|right|<reference|<merge|footnote-|<arg|num>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>