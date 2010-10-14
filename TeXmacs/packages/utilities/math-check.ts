<TeXmacs|1.0.7.7>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|math-check|1.0>

    <\src-purpose>
      Semantic checking of mathematical content
    </src-purpose>

    <src-copyright|2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(math math-markup)>

  <\active*>
    <\src-comment>
      Helper macros for presentation of errors
    </src-comment>
  </active*>

  <assign|math-nesting-mode|colored>

  <assign|math-error|<macro|x|<with|ornament-color|#ffc0c0|ornament-sunny-color|#ffe0e0|ornament-shadow-color|#ff8080|ornament-border|1px|ornament-hpadding|2px|ornament-vpadding|2px|<ornament|<arg|x>>>>>

  <drd-props|extern:math-check|with-like|true|arity|1|accessible|all>

  <drd-props|extern:math-check-table|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Redefine basic mathematical macros
    </src-comment>
  </active*>

  <assign|math|<macro|x|<with|mode|math|<extern|math-check|<quote-arg|x>>>>>

  <assign|equation*|<macro|body|<\with|mode|math|math-display|true|par-ver-sep|<eqn-ver-sep>>
    <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-short-above>><no-indent><htab|0fn>|<htab|0fn><vspace|<eqn-short-below>><no-indent*>|<extern|math-check|<quote-arg|body>>>>
  </with>>>

  <assign|equation-lab|<\macro|body|lab>
    <\surround|<set-binding|<arg|lab>>|<space|5mm><with|mode|text|font-shape|right|(<arg|lab>)>>
      <\with|mode|math|math-display|true|par-ver-sep|<eqn-ver-sep>>
        <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-short-above>><no-indent><htab|0fn>|<htab|0fn><vspace|<eqn-short-below>><no-indent*>|<extern|math-check|<quote-arg|body>>>>
      </with>
    </surround>
  </macro>>

  <assign|equation|<\macro|body>
    <\surround|<next-equation><set-binding|<the-equation>>|>
      <\surround||<space|5mm><with|mode|text|font-shape|right|(<the-equation>)>>
        <\with|mode|math|math-display|true|par-ver-sep|<eqn-ver-sep>>
          <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-short-above>><no-indent><htab|0fn>|<htab|0fn><vspace|<eqn-short-below>><no-indent*>|<extern|math-check|<quote-arg|body>>>>
        </with>
      </surround>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Redefine tabular mathematical macros
    </src-comment>
  </active*>

  <assign|eqnarray*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|b>|<cwith|1|-1|2|-2|cell-halign|c>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-block|no>|<extern|math-check-table|<quote-arg|body>>>
    </equations-base>
  </macro>>

  <assign|leqnarray*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|3>|<twith|table-max-cols|3>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|2|-2|cell-halign|c>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-block|no>|<extern|math-check-table|<quote-arg|body>>>
    </equations-base>
  </macro>>

  <assign|align*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|2>|<twith|table-max-cols|2>|<cwith|1|-1|1|1|cell-lsep|0spc>|<cwith|1|-1|1|1|cell-rsep|0.5spc>|<cwith|1|-1|-1|-1|cell-rsep|0spc>|<cwith|1|-1|-1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0sep>|<cwith|1|-1|1|-1|cell-tsep|0sep>|<cwith|1|-1|1|1|cell-hpart|1>|<cwith|1|-1|-1|-1|cell-hpart|1>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|b>|<cwith|1|-1|-1|-1|cell-halign|l>|<cwith|1|-1|-1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-block|no>|<extern|math-check-table|<quote-arg|body>>>
    </equations-base>
  </macro>>

  <assign|gather*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|1|-1|1|-1|cell-halign|c>|<cwith|1|-1|1|-1|cell-block|no>|<extern|math-check-table|<quote-arg|body>>>
    </equations-base>
  </macro>>

  <assign|eqsplit*|<\macro|body>
    <\equations-base>
      <tformat|<twith|table-hyphen|y>|<twith|table-width|1par>|<twith|table-min-cols|1>|<twith|table-max-cols|1>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|1|-1|1|-1|cell-bsep|0spc>|<cwith|1|-1|1|-1|cell-tsep|0spc>|<cwith|1|-1|1|-1|cell-hpart|1>|<cwith|1|-1|1|-1|cell-hyphen|b>|<cwith|-1|-1|1|1|cell-halign|r>|<cwith|2|-2|1|1|cell-halign|c>|<cwith|1|1|1|1|cell-halign|l>|<cwith|1|-1|1|-1|cell-block|no>|<extern|math-check-table|<quote-arg|body>>>
    </equations-base>
  </macro>>

  <assign|eqnarray|<value|eqnarray*>>

  <assign|leqnarray|<value|leqnarray*>>

  <assign|align|<value|align*>>

  <assign|gather|<value|gather*>>

  <assign|eqsplit|<value|eqsplit*>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>