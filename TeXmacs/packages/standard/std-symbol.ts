<TeXmacs|1.0.7.9>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-symbol|1.0>

    <\src-purpose>
      Some additional symbols for text mode. This file should become obsolete
      when better support for Unicode will be implemented.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|cent|<macro|<active*|<with|mode|text|font|tc|¢>>>>

  <assign|currency|<macro|<active*|<with|mode|text|font|tc|¤>>>>

  <assign|yen|<macro|<active*|<with|mode|text|font|tc|¥>>>>

  <assign|copyright|<macro|<active*|<with|mode|text|font|tcx|©>>>>

  <assign|copyleft|<macro|<active*|<with|mode|text|font|tcx|«>>>>

  <assign|registered|<macro|<active*|<with|mode|text|font|tcx|®>>>>

  <assign|degreesign|<macro|<active*|<with|mode|text|font|tc|°>>>>

  <assign|twosuperior|<macro|<active*|<with|mode|text|font|tc|²>>>>

  <assign|threesuperior|<macro|<active*|<with|mode|text|font|tc|³>>>>

  <assign|onesuperior|<macro|<active*|<with|mode|text|font|tc|¹>>>>

  <assign|mu|<macro|<active*|<with|mode|text|font|tcx|µ>>>>

  <assign|paragraphsign|<active*|<macro|<with|mode|text|font|tc|¶>>>>

  <assign|onequarter|<macro|<active*|<with|mode|text|font|tc|¼>>>>

  <assign|onehalf|<macro|<active*|<with|mode|text|font|tc|½>>>>

  <assign|threequarters|<macro|<active*|<with|mode|text|font|tc|¾>>>>

  <assign|euro|<macro|<active*|<with|mode|text|font|tcx|¿>>>>

  <assign|trademark|<macro|<active*|<with|mode|text|font|tcx|—>>>>

  <assign|emdash|<macro|<active*|<with|mode|text|font|roman|\V>>>>

  <assign|masculine|<active*|<rsup|<wide*|o|\<wide-bar\>>>>>

  <assign|ordfeminine|<active*|<rsup|<wide*|a|\<wide-bar\>>>>>

  <assign|varmasculine|<active*|<rsup|o>>>

  <assign|varordfeminine|<active*|<rsup|a>>>

  \;

  <assign|nbsp|<macro| <no-break><specific|screen|<resize|<move|<with|color|#A0A0FF|->|-0.3em|>|0em||0em|>>>>

  <assign|nbhyph|<macro|-<no-break>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
