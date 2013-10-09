<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|scilab|1.0>

    <\src-purpose>
      Markup for Scilab sessions.
    </src-purpose>

    <src-copyright|20013-|François Poulain>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|doc>

  <\active*>
    <\src-comment>
      Customize input/output fields
    </src-comment>
  </active*>

  <assign|scilab-prompt-color|dark green>

  <assign|scilab-input-color|black>

  <assign|scilab-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|scilab-prompt-color>|generic-input-color|<value|scilab-input-color>>
      <generic-input|<arg|prompt> |<arg|body>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>