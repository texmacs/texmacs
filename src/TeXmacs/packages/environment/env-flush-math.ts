<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-flush-math|1.0>

    <\src-purpose>
      Mathematical environments (equations and equation arrays) flushed to
      the left.
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

  <use-package|env-math>

  <\active*>
    <\src-comment>
      Styling parameters.
    </src-comment>
  </active*>

  <assign|eqn-left-indent|<macro|1.5fn>>

  <\active*>
    <\src-comment>
      Single equations.
    </src-comment>
  </active*>

  <assign|equation*|<macro|body|<\with|mode|math|math-display|true|par-ver-sep|<eqn-ver-sep>>
    <indent-left|<eqn-left-indent>|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-short-above>><no-indent>|<htab|0fn><vspace|<eqn-short-below>><no-indent*>|<arg|body>>>>
  </with>>>

  <assign|equation-lab|<\macro|body|lab>
    <\surround|<assign|the-label|<arg|lab>>|<htab|5mm><with|mode|text|(<arg|lab>)>>
      <\equation*>
        <arg|body>
      </equation*>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Equation arrays.
    </src-comment>
  </active*>

  <assign|equations-base|<\macro|body>
    <\with|mode|math|math-display|true|par-mode|left|par-sep|<eqn-row-sep>>
      <indent-left|<eqn-left-indent>|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-long-above>>|<vspace|<eqn-long-below>><no-indent*>|<arg|body>>>>
    </with>
  </macro>>

  <assign|old-eqnarray*|<value|eqnarray*>>

  <assign|eqnarray*|<\macro|body>
    <old-eqnarray*|<tformat|<cwith|1|-1|1|1|cell-hpart|0>|<cwith|1|-1|1|1|cell-width|0.5par>|<cwith|1|-1|1|1|cell-hmode|min>|<arg|body>>>
  </macro>>

  <assign|old-align*|<value|align*>>

  <assign|align*|<\macro|body>
    <old-align*|<tformat|<cwith|1|-1|1|1|cell-hpart|0>|<cwith|1|-1|1|1|cell-width|0.5par>|<cwith|1|-1|1|1|cell-hmode|min>|<arg|body>>>
  </macro>>

  <assign|old-gather*|<value|gather*>>

  <assign|gather*|<\macro|body>
    <old-gather*|<tformat|<cwith|1|-1|1|-1|cell-halign|l>|<arg|body>>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>