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
      Main checking macros based on external scheme routines
    </src-comment>
  </active*>

  <assign|math-error|<macro|x|<with|ornament-color|#ffc0c0|ornament-sunny-color|#ffe0e0|ornament-shadow-color|#ff8080|ornament-border|1px|ornament-hpadding|2px|ornament-vpadding|2px|<ornament|<arg|x>>>>>

  <assign|math-check|<macro|x|<extern|math-check|<arg|x>>>>

  <assign|math-check-table|<macro|x|<extern|math-check-table|<arg|x>>>>

  <drd-props|math-check|arity|1|accessible|all>

  <drd-props|math-check-table|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Customized mathematical structures
    </src-comment>
  </active*>

  <assign|math|<macro|x|<with|mode|math|<math-check|<arg|x>>>>>

  <assign|equation*|<macro|body|<\with|mode|math|math-display|true|par-ver-sep|<eqn-ver-sep>>
    <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-short-above>><no-indent><htab|0fn>|<htab|0fn><vspace|<eqn-short-below>><no-indent*>|<math-check|<arg|body>>>>
  </with>>>

  <assign|equations-base|<\macro|body>
    <\with|mode|math|math-display|true|par-mode|center|par-sep|<eqn-row-sep>>
      <style-with|src-compact|none|<surround|<no-page-break*><vspace*|<eqn-long-above>>|<vspace|<eqn-long-below>><no-indent*>|<math-check-table|<arg|body>>>>
    </with>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>