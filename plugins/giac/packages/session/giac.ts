<TeXmacs|2.1>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|giac|1.0>

    <\src-purpose>
      Markup for Giac sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|giac-prompt-color|dark brown>

  <assign|giac-input-color|dark blue>

  <assign|giac-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|giac-prompt-color>|generic-input-color|<value|giac-input-color>>
      <generic-input|<arg|prompt>|<arg|body>>
    </with>
  </macro>>

  <assign|giac-output|<\macro|body>
    <\padded>
      <generic-output*|<arg|body>>
    </padded>
  </macro>>

  <assign|giac-errput|<\macro|body>
    <\wide-normal>
      <with|color|dark green|font-family|ss|<arg|body>>
    </wide-normal>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>