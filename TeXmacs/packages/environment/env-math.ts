<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-math|1.0>

    <\src-purpose>
      Mathematical environments (equations and equation arrays).
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
      Numbering equations.
    </src-comment>
  </active*>

  <assign|next-number|<macro|<style-with|src-compact|none|<next-equation><with|mode|text|font-shape|right|(<the-equation>)>>>>

  <assign|leq-number|<macro|<next-number><htab|5mm>>>

  <assign|req-number|<macro|<htab|5mm><next-number>>>

  <assign|eq-number|<value|req-number>>

  <\active*>
    <\src-comment>
      Single equations.
    </src-comment>
  </active*>

  <assign|equation-lab|<macro|body|lab|<with|mode|math|math-display|true|par-sep|0.45fn|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|0.5fn><no-indent><assign|the-label|<arg|lab>><htab|5mm>|<htab|5mm>(<with|mode|text|<arg|lab>>)<vspace|0.5fn><no-indent*>|<arg|body>>>>>>

  <assign|equation|<macro|body|<with|mode|math|math-display|true|par-sep|0.45fn|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|0.5fn><no-indent><next-equation><htab|5mm>|<htab|5mm><with|mode|text|(<the-equation>)><vspace|0.5fn><no-indent*>|<arg|body>>>>>>

  <assign|equation*|<macro|body|<with|mode|math|math-display|true|par-sep|0.45fn|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|0.5fn><no-indent><htab|0fn>|<htab|0fn><vspace|0.5fn><no-indent*>|<arg|body>>>>>>

  <\active*>
    <\src-comment>
      Equation arrays.
    </src-comment>
  </active*>

  <assign|eqnarray*|<macro|body|<with|par-mode|center|mode|math|math-display|true|par-sep|0.45fn|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|b>|<cwith|1|-1|2|-2|cell-halign|c>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|body>>>>>>

  <assign|eqnarray|<value|eqnarray*>>

  <assign|leqnarray*|<macro|body|<with|mode|math|math-display|true|par-sep|0.45fn|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|-2|cell-halign|c>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|1|1|cell-width|1.5fn>|<cwith|1|-1|2|2|cell-width|1fn>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|body>>>>>>

  <assign|leqnarray|<value|leqnarray*>>

  <assign|align*|<macro|body|<with|par-mode|center|mode|math|math-display|true|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|2>|<twith|table-max-cols|2>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0.5spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|-1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|b>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<arg|body>>>>>>

  <assign|align|<value|align*>>

  <assign|gather*|<macro|body|<with|par-mode|center|mode|math|math-display|true|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|1|-1|1|-1|cell-halign|c>|<arg|body>>>>>>

  <assign|gather|<value|gather*>>

  <assign|eqsplit*|<macro|body|<with|par-mode|center|mode|math|math-display|true|<surround|<no-page-break*><vspace*|0.5fn>|<vspace|0.5fn><no-indent*>|<tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|-1|-1|1|1|cell-halign|r>|<cwith|2|-2|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-halign|l>|<arg|body>>>>>>

  <assign|eqsplit|<value|eqsplit*>>

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