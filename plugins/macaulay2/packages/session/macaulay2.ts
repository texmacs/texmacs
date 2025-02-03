<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|macaulay2|1.0>

    <\src-purpose>
      Markup for Macaulay2 sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|macaulay2-prompt-color|dark green>

  <assign|macaulay2-input-color|blue>

  <assign|macaulay2-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|macaulay2-prompt-color>|generic-input-color|<value|macaulay2-input-color>>
      <generic-input|<arg|prompt>|<arg|body>>
    </with>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>