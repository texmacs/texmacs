<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|pari|1.0>

    <\src-purpose>
      Markup for Pari sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|pari-prompt-color|dark brown>

  <assign|pari-input-color|dark green>

  <assign|pari-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|pari-prompt-color>|generic-input-color|<value|pari-input-color>>
      <generic-input|<arg|prompt>|<arg|body>>
    </with>
  </macro>>

  <assign|pari-output|<macro|body|<style-with|src-compact|none|<generic-output|<with|math-display|false|<arg|body>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>