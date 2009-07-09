<TeXmacs|1.0.7.2>

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

  <use-package|tmdoc>

  <\active*>
    <\src-comment>
      Customize input/output fields
    </src-comment>
  </active*>

  <assign|mmx-prompt|<macro|nr|<with|color|red|<arg|nr>]<specific|html|&nbsp;>
  >>>

  <assign|mmx-prompt|<macro|nr|<with|mode|math|Mmx\<rangle\><specific|html|&nbsp;>
  \ >>>

  <assign|mmx-prompt|<macro|nr|<with|prog-language|verbatim|Mmx]<specific|html|&nbsp;>
  >>>

  <assign|mathemagix-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<with|color|red|<arg|prompt>>|<with|color|dark
  brown|<arg|body>>>>>>

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