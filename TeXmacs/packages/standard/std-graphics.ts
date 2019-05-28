<TeXmacs|1.99.9>

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

  <assign|draw-over|<macro|bg|fg|pad|<with|dims|<box-info|<gr-tbl|<arg|bg>>|wh>|cm-1|<over|<look-up|<cm-length>|0>>|<with|ww|<look-up|<value|dims>|0>|hh|<look-up|<value|dims>|1>|pd|<over|<arg|pad>|0.5tmpt>|<with|gr-geometry|<gr-geom|<tuple|<plus|<value|ww>|<value|pd>>|<plus|<value|hh>|<value|pd>>>|<value|cm-1>>|gr-frame|<tuple|scale|<merge|<value|magnification>|cm>|<tuple|0.5gw|0.5gh>>|<style-with|src-compact|none|<superpose|<freeze|<gr-tbl|<if|<equal|<quote-arg|bg>|>|<flag|draw
  over|blue>><arg|bg>>>|<resize|<move|<gr-tbl|<arg|fg>>|<minus|<arg|pad>>|>|0|<plus|0.5ex|<times|<value|hh>|-0.5tmpt>>|<times|<value|ww>|1tmpt>|<plus|0.5ex|<times|<value|hh>|0.5tmpt>>>>>>>>>>

  <assign|draw-under|<macro|bg|fg|pad|<with|dims|<box-info|<gr-tbl|<arg|bg>>|wh>|cm-1|<over|<look-up|<cm-length>|0>>|<with|ww|<look-up|<value|dims>|0>|hh|<look-up|<value|dims>|1>|pd|<over|<arg|pad>|0.5tmpt>|<with|gr-geometry|<gr-geom|<tuple|<plus|<value|ww>|<value|pd>>|<plus|<value|hh>|<value|pd>>>|<value|cm-1>>|gr-frame|<tuple|scale|<merge|<value|magnification>|cm>|<tuple|0.5gw|0.5gh>>|<style-with|src-compact|none|<superpose|<gr-tbl|<if|<equal|<quote-arg|bg>|>|<flag|draw
  over|blue>><arg|bg>>|<freeze|<resize|<move|<gr-tbl|<arg|fg>>|<minus|<arg|pad>>|>|0|<plus|0.5ex|<times|<value|hh>|-0.5tmpt>>|<times|<value|ww>|1tmpt>|<plus|0.5ex|<times|<value|hh>|0.5tmpt>>>>>>>>>>>

  <drd-props|draw-over|arity|3|length|2>

  <drd-props|draw-under|arity|3|length|2>

  <\active*>
    <\src-comment>
      Paragraph boxes for inside text-at boxes
    </src-comment>
  </active*>

  <assign|hpar-length|<macro|<over|<minus|1par|<value|par-columns-sep>>|2>>>

  <assign|paragraph-box|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|<value|doc-at-padding>>|<cwith|1|1|1|1|cell-rsep|<value|doc-at-padding>>|<cwith|1|1|1|1|cell-bsep|<value|doc-at-padding>>|<cwith|1|1|1|1|cell-tsep|<value|doc-at-padding>>|<twith|table-width|<value|doc-at-width>>|<twith|table-hmode|<value|doc-at-hmode>>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-background|<value|fill-color>>|<cwith|1|1|1|1|cell-tborder|<value|doc-at-border>>|<cwith|1|1|1|1|cell-bborder|<value|doc-at-border>>|<cwith|1|1|1|1|cell-lborder|<value|doc-at-border>>|<cwith|1|1|1|1|cell-rborder|<value|doc-at-border>>|<table|<row|<\cell>
    <\with|par-par-sep|<if|<equal|<value|doc-at-ppsep>|>|<value|par-par-sep>|<value|doc-at-ppsep>>>
      <arg|body>
    </with>
  </cell>>>>>>>

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

  <assign|arrow-with-text|<macro|p1|p2|t|<extern|arrow-with-text|<arg|p1>|<arg|p2>|<quote-arg|t>>>>

  <assign|arrow-with-text*|<macro|p1|p2|t|<extern|arrow-with-text*|<arg|p1>|<arg|p2>|<quote-arg|t>>>>

  <drd-props|arrow-with-text|arity|3|accessible|2>

  <drd-props|arrow-with-text*|arity|3|accessible|2>

  <\active*>
    <\src-comment>
      Transformations
    </src-comment>
  </active*>

  <assign|rotate|<macro|angle|body|<gr-transform|<arg|body>|<tuple|rotation|<arg|angle>>>>>

  <assign|rotate-at|<macro|angle|x|y|body|<gr-transform|<arg|body>|<tuple|rotation|<point|<over|<arg|x>|1tmpt>|<over|<arg|y>|1tmpt>>|<arg|angle>>>>>

  <assign|dilate|<macro|x-factor|y-factor|body|<gr-transform|<arg|body>|<tuple|scaling|<arg|x-factor>|<arg|y-factor>>>>>

  <assign|skew|<macro|slant|body|<gr-transform|<arg|body>|<tuple|slanting|<arg|slant>>>>>

  <assign|linear-2d|<macro|a|b|c|d|body|<gr-transform|<arg|body>|<tuple|linear|<arg|a>|<arg|b>|<arg|c>|<arg|d>>>>>

  <\active*>
    <\src-comment>
      Blur effect
    </src-comment>
  </active*>

  <assign|blur-pen-width|1ln>

  <assign|blur-pen-height|1ln>

  <assign|blur-pen-angle|0>

  <assign|blur-pen-dx|1ln>

  <assign|blur-pen-dy|1ln>

  \;

  <assign|blur|<macro|body|<gaussian-blur|<arg|body>>>>

  <assign|gaussian-blur|<macro|body|<gr-effect|<arg|body>|<eff-blur|0|<eff-gaussian|<value|blur-pen-width>|<value|blur-pen-height>|<over|<value|blur-pen-angle>|57.2957795131>>>>>>

  <assign|oval-blur|<macro|body|<gr-effect|<arg|body>|<eff-blur|0|<eff-oval|<value|blur-pen-width>|<value|blur-pen-height>|<over|<value|blur-pen-angle>|57.2957795131>>>>>>

  <assign|rectangular-blur|<macro|body|<gr-effect|<arg|body>|<eff-blur|0|<eff-rectangular|<value|blur-pen-width>|<value|blur-pen-height>|<over|<value|blur-pen-angle>|57.2957795131>>>>>>

  <assign|motion-blur|<macro|body|<gr-effect|<arg|body>|<eff-blur|0|<eff-motion|<value|blur-pen-dx>|<value|blur-pen-dy>>>>>>

  <\active*>
    <\src-comment>
      Outline effect
    </src-comment>
  </active*>

  <assign|outline-pen-width|0.5ln>

  <assign|outline-pen-height|0.5ln>

  <assign|outline-pen-angle|0>

  <assign|outline-pen-dx|1ln>

  <assign|outline-pen-dy|1ln>

  \;

  <assign|outline|<macro|body|<oval-outline|<arg|body>>>>

  <assign|gaussian-outline|<macro|body|<gr-effect|<arg|body>|<eff-outline|0|<eff-gaussian|<value|outline-pen-width>|<value|outline-pen-height>|<over|<value|outline-pen-angle>|57.2957795131>>>>>>

  <assign|oval-outline|<macro|body|<gr-effect|<arg|body>|<eff-outline|0|<eff-oval|<value|outline-pen-width>|<value|outline-pen-height>|<over|<value|outline-pen-angle>|57.2957795131>>>>>>

  <assign|rectangular-outline|<macro|body|<gr-effect|<arg|body>|<eff-outline|0|<eff-rectangular|<value|outline-pen-width>|<value|outline-pen-height>|<over|<value|outline-pen-angle>|57.2957795131>>>>>>

  <assign|motion-outline|<macro|body|<gr-effect|<arg|body>|<eff-outline|0|<eff-motion|<value|outline-pen-dx>|<value|outline-pen-dy>>>>>>

  <\active*>
    <\src-comment>
      Thicken effect
    </src-comment>
  </active*>

  <assign|thicken-pen-width|1ln>

  <assign|thicken-pen-height|1ln>

  <assign|thicken-pen-angle|0>

  <assign|thicken-pen-dx|1ln>

  <assign|thicken-pen-dy|1ln>

  \;

  <assign|thicken|<macro|body|<rectangular-thicken|<arg|body>>>>

  <assign|gaussian-thicken|<macro|body|<gr-effect|<arg|body>|<eff-thicken|0|<eff-gaussian|<value|thicken-pen-width>|<value|thicken-pen-height>|<over|<value|thicken-pen-angle>|57.2957795131>>>>>>

  <assign|oval-thicken|<macro|body|<gr-effect|<arg|body>|<eff-thicken|0|<eff-oval|<value|thicken-pen-width>|<value|thicken-pen-height>|<over|<value|thicken-pen-angle>|57.2957795131>>>>>>

  <assign|rectangular-thicken|<macro|body|<gr-effect|<arg|body>|<eff-thicken|0|<eff-rectangular|<value|thicken-pen-width>|<value|thicken-pen-height>|<over|<value|thicken-pen-angle>|57.2957795131>>>>>>

  <assign|motion-thicken|<macro|body|<gr-effect|<arg|body>|<eff-thicken|0|<eff-motion|<value|thicken-pen-dx>|<value|thicken-pen-dy>>>>>>

  <\active*>
    <\src-comment>
      Erode effect
    </src-comment>
  </active*>

  <assign|erode-pen-width|0.5ln>

  <assign|erode-pen-height|0.5ln>

  <assign|erode-pen-angle|0>

  <assign|erode-pen-dx|1ln>

  <assign|erode-pen-dy|1ln>

  \;

  <assign|erode|<macro|body|<rectangular-erode|<arg|body>>>>

  <assign|gaussian-erode|<macro|body|<gr-effect|<arg|body>|<eff-erode|0|<eff-gaussian|<value|erode-pen-width>|<value|erode-pen-height>|<over|<value|erode-pen-angle>|57.2957795131>>>>>>

  <assign|oval-erode|<macro|body|<gr-effect|<arg|body>|<eff-erode|0|<eff-oval|<value|erode-pen-width>|<value|erode-pen-height>|<over|<value|erode-pen-angle>|57.2957795131>>>>>>

  <assign|rectangular-erode|<macro|body|<gr-effect|<arg|body>|<eff-erode|0|<eff-rectangular|<value|erode-pen-width>|<value|erode-pen-height>|<over|<value|erode-pen-angle>|57.2957795131>>>>>>

  <assign|motion-erode|<macro|body|<gr-effect|<arg|body>|<eff-erode|0|<eff-motion|<value|erode-pen-dx>|<value|erode-pen-dy>>>>>>

  <\active*>
    <\src-comment>
      Further effects
    </src-comment>
  </active*>

  <assign|shadow-dx|-1ln>

  <assign|shadow-dy|-1ln>

  <assign|shadow-color|grey>

  <assign|shadow-blur-radius|1ln>

  <assign|eff-shadow|<macro|eff|dx|dy|col|r|<eff-blur|<eff-monochrome|<eff-move|0|<arg|dx>|<arg|dy>>|<arg|col>|1>|<eff-gaussian|<arg|r>>>>>

  <assign|shadow|<macro|body|<gr-effect|<arg|body>|<eff-superpose|<eff-blur|<eff-monochrome|<eff-move|0|<minus|<value|shadow-dx>>|<minus|<value|shadow-dy>>>|<value|shadow-color>|1>|<eff-gaussian|<value|shadow-blur-radius>>>|0>>>>

  <assign|shadowed-raise|<macro|body|<gr-effect|<arg|body>|<eff-superpose|<eff-blur|<eff-monochrome|0|<value|shadow-color>|1>|<eff-gaussian|<value|shadow-blur-radius>>>|<eff-move|0|<value|shadow-dx>|<value|shadow-dy>>>>>>

  \;

  <assign|engrave-dx|0.5ln>

  <assign|engrave-dy|0.5ln>

  <assign|engrave-color|grey>

  <assign|engrave|<macro|body|<gr-effect|<arg|body>|<eff-superpose|0|<eff-monochrome|<eff-sub|0|<eff-erode|0|<eff-motion|<value|engrave-dx>|<value|engrave-dy>>>>|<value|engrave-color>|1>>>>>

  <assign|outlined-engrave|<macro|body|<gr-effect|<arg|body>|<eff-superpose|<eff-outline|0|<eff-oval|<value|outline-pen-width>|<value|outline-pen-height>|<over|<value|outline-pen-angle>|57.2957795131>>>|<eff-erode|0|<eff-motion|<value|engrave-dx>|<value|engrave-dy>>>>>>>

  \;

  <assign|emboss-dx|-1.5ln>

  <assign|emboss-dy|-1.5ln>

  <assign|emboss-color|grey>

  <assign|emboss|<macro|body|<gr-effect|<arg|body>|<eff-superpose|<eff-monochrome|<eff-thicken|0|<eff-motion|<value|emboss-dx>|<value|emboss-dy>>>|<value|emboss-color>|1>|<eff-move|0|<value|emboss-dx>|<value|emboss-dy>>>>>>

  <assign|outlined-emboss|<macro|body|<gr-effect|<arg|body>|<eff-superpose|<eff-outline|<eff-thicken|0|<eff-motion|<value|emboss-dx>|<value|emboss-dy>>>|<eff-oval|<value|outline-pen-width>|<value|outline-pen-height>|<over|<value|outline-pen-angle>|57.2957795131>>>|<eff-move|0|<value|emboss-dx>|<value|emboss-dy>>>>>>

  <\active*>
    <\src-comment>
      Degradation, distortion and gnawing
    </src-comment>
  </active*>

  <assign|degrade-wavelen-x|0.25fn>

  <assign|degrade-wavelen-y|0.25fn>

  <assign|degrade-threshold|0.667>

  <assign|degrade-sharpness|1.0>

  <assign|degrade|<macro|body|<gr-effect|<arg|body>|<eff-degrade|0|<value|degrade-wavelen-x>|<value|degrade-wavelen-y>|<value|degrade-threshold>|<value|degrade-sharpness>>>>>

  \;

  <assign|distort-wavelen-x|0.25fn>

  <assign|distort-wavelen-y|0.25fn>

  <assign|distort-radius-x|0.1fn>

  <assign|distort-radius-y|0.1fn>

  <assign|distort|<macro|body|<gr-effect|<arg|body>|<eff-distort|0|<value|distort-wavelen-x>|<value|distort-wavelen-y>|<value|distort-radius-x>|<value|distort-radius-y>>>>>

  \;

  <assign|gnaw-wavelen-x|0.25fn>

  <assign|gnaw-wavelen-y|0.25fn>

  <assign|gnaw-radius-x|0.1fn>

  <assign|gnaw-radius-y|0.1fn>

  <assign|gnaw|<macro|body|<gr-effect|<arg|body>|<eff-gnaw|0|<value|gnaw-wavelen-x>|<value|gnaw-wavelen-y>|<value|gnaw-radius-x>|<value|gnaw-radius-y>>>>>

  <\active*>
    <\src-comment>
      Miscellaneous effects
    </src-comment>
  </active*>

  <assign|burning|<macro|body|<gr-effect|<arg|body>|<eff-superpose|<eff-shadow|0|0.1ex|0.9ex|#f20|0.9ex>|<eff-shadow|0|-0.1ex|0.75ex|#f80|0.55ex>|<eff-shadow|0|0.2ex|0.5ex|#fd3|0.3ex>|<eff-shadow|0|0ex|0.25ex|#ff3|0.2ex>|<eff-shadow|0|0ex|0ex|#ccc|0.2ex>|0>>>>

  <assign|bubble|<macro|body|r|lambda|<gr-effect|<arg|body>|<eff-bubble|0|<arg|r>|<arg|lambda>>>>>

  <assign|turbulence|<macro|body|seed|w|h|octaves|<gr-effect|<arg|body>|<eff-turbulence|0|<arg|seed>|<arg|w>|<arg|h>|<arg|octaves>>>>>

  <assign|fractal-noise|<macro|body|seed|w|h|octaves|<gr-effect|<arg|body>|<eff-fractal-noise|0|<arg|seed>|<arg|w>|<arg|h>|<arg|octaves>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>