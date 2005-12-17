<TeXmacs|1.0.6>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-graphics|1.0>

    <\src-purpose>
      Macros for graphics.
    </src-purpose>

    <src-copyright|2005|Henri Lesourd>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Drawing over background.
    </src-comment>
  </active*>

  <assign|gr-geom|<macro|dims|cm-1|<style-with|src-compact|none|<tuple|geometry|<merge|<times|<look-up|<arg|dims>|0>|<arg|cm-1>>|cm>|<merge|<times|<look-up|<arg|dims>|1>|<arg|cm-1>>|cm>>>>>

  <assign|gr-tbl|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-bsep|0pt>|<cwith|1|1|1|1|cell-tsep|0pt>|<cwith|1|1|1|1|cell-lborder|0pt>|<cwith|1|1|1|1|cell-rborder|0pt>|<cwith|1|1|1|1|cell-bborder|0pt>|<cwith|1|1|1|1|cell-tborder|0pt>|<table|<row|<cell|<arg|x>>>>>>>>

  <assign|with-background|<macro|bg|fg|<with|dims|<box-info|<gr-tbl|<arg|bg>>|wh>|cm-1|<over|<look-up|<cm-length>|0>>|<with|gr-geometry|<gr-geom|<value|dims>|<value|cm-1>>|<style-with|src-compact|none|<superpose|<frozen|<gr-tbl|<arg|bg>>>|<gr-tbl|<arg|fg>>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>