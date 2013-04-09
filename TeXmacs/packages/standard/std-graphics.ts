<TeXmacs|1.0.7.19>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-graphics|1.0>

    <\src-purpose>
      Macros for graphics.
    </src-purpose>

    <src-copyright|2005--2012|Joris van der Hoeven, Henri Lesourd>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Drawing over background.
    </src-comment>
  </active*>

  <assign|gr-geom|<macro|dims|cm-1|<style-with|src-compact|none|<tuple|geometry|<merge|<times|<look-up|<arg|dims>|0>|<arg|cm-1>>|cm>|<merge|<times|<look-up|<arg|dims>|1>|<arg|cm-1>>|cm>>>>>

  <assign|gr-tbl|<macro|x|<block|<tformat|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-bsep|0pt>|<cwith|1|1|1|1|cell-tsep|0pt>|<cwith|1|1|1|1|cell-lborder|0pt>|<cwith|1|1|1|1|cell-rborder|0pt>|<cwith|1|1|1|1|cell-bborder|0pt>|<cwith|1|1|1|1|cell-tborder|0pt>|<table|<row|<cell|<arg|x>>>>>>>>

  <assign|draw-over|<macro|bg|fg|<with|dims|<box-info|<gr-tbl|<arg|bg>>|wh>|cm-1|<over|<look-up|<cm-length>|0>>|<with|gr-geometry|<gr-geom|<value|dims>|<value|cm-1>>|<style-with|src-compact|none|<superpose|<freeze|<gr-tbl|<arg|bg>>>|<gr-tbl|<arg|fg>>>>>>>>

  <assign|draw-under|<macro|bg|fg|<with|dims|<box-info|<gr-tbl|<arg|bg>>|wh>|cm-1|<over|<look-up|<cm-length>|0>>|<with|gr-geometry|<gr-geom|<value|dims>|<value|cm-1>>|<style-with|src-compact|none|<superpose|<gr-tbl|<arg|bg>>|<freeze|<gr-tbl|<arg|fg>>>>>>>>>

  <\active*>
    <\src-comment>
      Predefined useful objects.
    </src-comment>
  </active*>

  <assign|rectangle|<\macro|p1|p2>
    <cline|<arg|p1>|<point|<look-up|<arg|p1>|0>|<look-up|<arg|p2>|1>>|<arg|p2>|<point|<look-up|<arg|p2>|0>|<look-up|<arg|p1>|1>>>
  </macro>>

  <assign|circle|<\macro|p1|p2>
    <\with|x1|<look-up|<arg|p1>|0>|y1|<look-up|<arg|p1>|1>>
      <\with|x2|<look-up|<arg|p2>|0>|y2|<look-up|<arg|p2>|1>>
        <\with|xr|<minus|<value|x2>|<value|x1>>|yr|<minus|<value|y2>|<value|y1>>>
          <carc|<arg|p2>|<point|<plus|<value|x1>|<minus|<value|xr>>>|<plus|<value|y1>|<minus|<value|yr>>>>|<point|<plus|<value|x1>|<minus|<value|yr>>>|<plus|<value|y1>|<value|xr>>>>
        </with>
      </with>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Effects
    </src-comment>
  </active*>

  <assign|blur|<macro|body|r|<gr-effect|<arg|body>|<eff-blur|0|<eff-gaussian|<arg|r>>>>>>

  <assign|gaussian-blur|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-blur|0|<eff-gaussian|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|oval-blur|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-blur|0|<eff-oval|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|rectangular-blur|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-blur|0|<eff-rectangular|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|motion-blur|<macro|body|dx|dy|<gr-effect|<arg|body>|<eff-blur|0|<eff-motion|<arg|dx>|<arg|dy>>>>>>

  \;

  <assign|outline|<macro|body|r|<gr-effect|<arg|body>|<eff-outline|0|<eff-oval|<arg|r>>>>>>

  <assign|gaussian-outline|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-outline|0|<eff-gaussian|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|oval-outline|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-outline|0|<eff-oval|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|rectangular-outline|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-outline|0|<eff-rectangular|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|motion-outline|<macro|body|dx|dy|<gr-effect|<arg|body>|<eff-outline|0|<eff-motion|<arg|dx>|<arg|dy>>>>>>

  \;

  <assign|thicken|<macro|body|rx|ry|<gr-effect|<arg|body>|<eff-thicken|0|<eff-rectangular|<arg|rx>|<arg|ry>|0>>>>>

  <assign|gaussian-thicken|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-thicken|0|<eff-gaussian|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|oval-thicken|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-thicken|0|<eff-oval|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|rectangular-thicken|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-thicken|0|<eff-rectangular|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|motion-thicken|<macro|body|dx|dy|<gr-effect|<arg|body>|<eff-thicken|0|<eff-motion|<arg|dx>|<arg|dy>>>>>>

  \;

  <assign|erode|<macro|body|rx|ry|<gr-effect|<arg|body>|<eff-erode|0|<eff-rectangular|<arg|rx>|<arg|ry>|0>>>>>

  <assign|gaussian-erode|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-erode|0|<eff-gaussian|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|oval-erode|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-erode|0|<eff-oval|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|rectangular-erode|<macro|body|rx|ry|phi|<gr-effect|<arg|body>|<eff-erode|0|<eff-rectangular|<arg|rx>|<arg|ry>|<arg|phi>>>>>>

  <assign|motion-erode|<macro|body|dx|dy|<gr-effect|<arg|body>|<eff-erode|0|<eff-motion|<arg|dx>|<arg|dy>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>