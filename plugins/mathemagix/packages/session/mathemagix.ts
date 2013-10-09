<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mathemagix|1.0>

    <\src-purpose>
      Markup for Mathemagix sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

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

  <assign|mathemagix-prompt-color|red>

  <assign|mathemagix-input-color|dark brown>

  <assign|mmx-prompt|<macro|nr|<with|color|red|<arg|nr>]<specific|html|&nbsp;>
  >>>

  <assign|mmx-prompt|<macro|nr|<with|mode|math|Mmx\<rangle\><specific|html|&nbsp;>
  \ >>>

  <assign|mmx-prompt|<macro|nr|<with|prog-language|verbatim|Mmx]<specific|html|&nbsp;>
  >>>

  <assign|mathemagix-input|<\macro|prompt|body>
    <\with|generic-prompt-color|<value|mathemagix-prompt-color>|generic-input-color|<value|mathemagix-input-color>>
      <generic-input|<arg|prompt>|<arg|body>>
    </with>
  </macro>>

  <assign|mathemagix-output|<macro|body|<generic-output|<with|prog-language|verbatim|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Other macros and environments.
    </src-comment>
  </active*>

  <assign|text|<macro|body|<with|mode|text|par-mode|justify|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>