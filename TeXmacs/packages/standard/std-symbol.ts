<TeXmacs|1.0.4.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|std-symbol|1.0>

    <\src-purpose>
      Some additional symbols for text mode. This file should become obsolete
      when better support for Unicode will be implemented.
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

  <assign|cent|<macro|<active*|<with|font|tc|¢>>>>

  <assign|currency|<macro|<active*|<with|font|tc|¤>>>>

  <assign|yen|<macro|<active*|<with|font|tc|¥>>>>

  <assign|copyright|<macro|<active*|<with|font|tcx|©>>>>

  <assign|copyleft|<macro|<active*|<with|font|tcx|«>>>>

  <assign|registered|<macro|<active*|<with|font|tcx|®>>>>

  <assign|degreesign|<macro|<active*|<with|font|tc|°>>>>

  <assign|twosuperior|<macro|<active*|<with|font|tc|²>>>>

  <assign|threesuperior|<macro|<active*|<with|font|tc|³>>>>

  <assign|onesuperior|<macro|<active*|<with|font|tc|¹>>>>

  <assign|mu|<macro|<active*|<with|font|tcx|µ>>>>

  <assign|paragraphsign|<active*|<macro|<with|font|tc|¶>>>>

  <assign|onequarter|<macro|<active*|<with|font|tc|¼>>>>

  <assign|onehalf|<macro|<active*|<with|font|tc|½>>>>

  <assign|threequarters|<macro|<active*|<with|font|tc|¾>>>>

  <assign|euro|<macro|<active*|<with|font|tcx|¿>>>>

  <assign|trademark|<macro|<active*|<with|font|tcx|—>>>>

  <assign|emdash|<macro|<active*|<with|font|roman|\V>>>>

  <assign|masculine|<active*|<rsup|<wide*|o|\<wide-bar\>>>>>

  <assign|ordfeminine|<active*|<rsup|<wide*|a|\<wide-bar\>>>>>

  <assign|varmasculine|<active*|<rsup|o>>>

  <assign|varordfeminine|<active*|<rsup|a>>>

  \;

  <assign|nbsp|<macro|<style-with|src-compact|none|
  <no-break><specific|screen|<resize|<move|<with|color|#8080FF|->|-0.3em|>|0em||0em||>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>