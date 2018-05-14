<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-math|1.0>

    <\src-purpose>
      Mathematical environments (equations and equation arrays).
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
      Styling parameters.

      \ \ eqn-short-above + par-sep: smallest skip above equation (no
      collisions).

      \ \ eqn-short-above + eqn-ver-sep: largest skip above equation (when
      collisions).

      \ \ eqn-long-above + par-row-sep: skip above equation array.

      \ \ eqn-short-below and eqn-long-below: similar to above.
    </src-comment>
  </active*>

  <assign|eqn-short-above|<macro|0.35fn>>

  <assign|eqn-short-below|<macro|0.35fn>>

  <assign|eqn-long-above|<macro|0.5fn>>

  <assign|eqn-long-below|<macro|0.5fn>>

  <assign|eqn-ver-sep|<macro|0.6fn>>

  <assign|eqn-row-sep|<macro|0.45fn>>

  <\active*>
    <\src-comment>
      Numbering equations.
    </src-comment>
  </active*>

  <assign|next-number|<macro|<style-with|src-compact|none|<next-equation><with|mode|text|font-shape|right|(<the-equation>)>>>>

  <drd-props|next-number|syntax|<macro|>>

  <assign|leq-number|<macro|<next-number><htab|5mm>>>

  <assign|req-number|<macro|<htab|5mm><next-number>>>

  <assign|eq-number|<value|req-number>>

  <assign|eq-lab|<macro|lab|<htab|5mm><with|mode|text|font-shape|right|(<arg|lab>)>>>

  <assign|no-number|<macro|>>

  <assign|eqref|<macro|id|(<reference|<arg|id>>)>>

  <\active*>
    <\src-comment>
      Single equations.
    </src-comment>
  </active*>

  <assign|equation*|<macro|body|<\with|mode|math|math-display|true|par-ver-sep|<eqn-ver-sep>>
    <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-short-above>><no-indent><htab|0fn>|<htab|0fn><vspace|<eqn-short-below>><no-indent*>|<arg|body>>>
  </with>>>

  <assign|equation-lab|<\macro|body|lab>
    <\surround|<set-binding|<arg|lab>>|<space|5mm><with|mode|text|font-shape|right|(<arg|lab>)>>
      <\equation*>
        <arg|body>
      </equation*>
    </surround>
  </macro>>

  <assign|equation|<\macro|body>
    <\surround|<next-equation>|>
      <\equation-lab>
        <arg|body>
      </equation-lab|<the-equation>>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Equation arrays.
    </src-comment>
  </active*>

  <assign|equations-base|<\macro|body>
    <\with|mode|math|math-display|true|par-mode|center|par-sep|<eqn-row-sep>|par-first|0em|par-swell|<value|math-swell>>
      <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-long-above>>|<vspace|<eqn-long-below>><no-indent*>|<arg|body>>>
    </with>
  </macro>>

  <assign|eqnarray*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|b>|<cwith|1|-1|2|-2|cell-halign|c>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-block|no>|<arg|body>>
    </equations-base>
  </macro>>

  <assign|eqnarray|<\macro|body>
    <eqnarray*|<tformat|<arg|body>>>
  </macro>>

  <assign|leqnarray*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|-2|cell-halign|c>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-block|no>|<arg|body>>
    </equations-base>
  </macro>>

  <assign|leqnarray|<\macro|body>
    <leqnarray*|<tformat|<arg|body>>>
  </macro>>

  <assign|align*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|2>|<twith|table-max-cols|2>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0.5spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|-1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|b>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-block|no>|<arg|body>>
    </equations-base>
  </macro>>

  <assign|align|<\macro|body>
    <align*|<tformat|<arg|body>>>
  </macro>>

  <assign|alignat*|<\macro|ncol|body>
    <align*|<tformat|<arg|body>>>
  </macro>>

  <assign|alignat|<\macro|ncol|body>
    <align|<tformat|<arg|body>>>
  </macro>>

  <assign|aligned*|<\macro|body>
    <align*|<tformat|<arg|body>>>
  </macro>>

  <assign|aligned|<\macro|body>
    <aligned*|<tformat|<arg|body>>>
  </macro>>

  <assign|alignedat*|<\macro|ncol|body>
    <align*|<tformat|<arg|body>>>
  </macro>>

  <assign|alignedat|<\macro|ncol|body>
    <align|<tformat|<arg|body>>>
  </macro>>

  <assign|flalign*|<\macro|body>
    <align*|<tformat|<arg|body>>>
  </macro>>

  <assign|flalign|<\macro|body>
    <align|<tformat|<arg|body>>>
  </macro>>

  <assign|multline*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|1|1|1|-1|cell-halign|l>|<cwith|2|-1|1|-1|cell-halign|r>|<cwith|1|-1|1|-1|cell-block|no>|<arg|body>>
    </equations-base>
  </macro>>

  <assign|multline|<\macro|body>
    <multline*|<tformat|<arg|body>>>
  </macro>>

  <assign|gather*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-block|no>|<arg|body>>
    </equations-base>
  </macro>>

  <assign|gather|<\macro|body>
    <gather*|<tformat|<arg|body>>>
  </macro>>

  <assign|eqsplit*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|-1|-1|1|1|cell-halign|r>|<cwith|2|-2|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-halign|l>|<cwith|1|-1|1|-1|cell-block|no>|<arg|body>>
    </equations-base>
  </macro>>

  <assign|eqsplit|<\macro|body>
    <eqsplit*|<tformat|<arg|body>>>
  </macro>>

  <assign|subequations|<\macro|body>
    <arg|body>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>