<TeXmacs|1.0.6.14>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mathemagix|1.0>

    <\src-purpose>
      Markup for Mathemagix sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|mmx-prompt|<macro|nr|<with|color|red|<arg|nr>]<specific|html|&nbsp;>
  >>>

  <assign|mathemagix-output|<macro|body|<surround|<vspace*|0.5fn>|<vspace|0fn>|<generic-output|<arg|body>>>>>

  <assign|generic-inputx|<macro|prompt|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-lsep|0.5fn>|<cwith|1|1|2|2|cell-rsep|0.5fn>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|2|2|cell-lsep|0fn>|<cwith|1|1|2|2|cell-tsep|0.25fn>|<cwith|1|1|2|2|cell-bsep|0.25fn>|<table|<row|<cell|<id-function|<arg|prompt>>>|<\cell>
    <with|math-display|true|<arg|body>>
  </cell>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>