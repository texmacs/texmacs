<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Specification of style properties>

  Each of the fundamental types of graphical objects also admits a certain
  number of style properties which affect the rendering. The following style
  properties exist:

  <paragraph|Color>This property applies to any of the graphical object types
  and specifies the color.

  <paragraph|Fill color>This property applies to all graphical object types
  except text and mathematics. It specifies a fill color for the object.

  <\big-figure|<with|gr-mode|<tuple|edit|cspline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-fill-color|pastel
  red|gr-color|dark magenta|gr-auto-crop|true|<graphics||<cspline|<point|-4.82503|-0.183391>|<point|-4.54985778542135|2.12379282973938>|<point|-3.34334898796137|1.57345548352957>|<point|-2.623677073687|0.070611191956608>|<point|-3.89368633417119|0.790283106230983>>|<with|fill-color|pastel
  yellow|<cspline|<point|-1.75584|2.22963>|<point|-2.24267429554174|1.23478634740045>|<point|-1.62883648630771|0.324613044053446>|<point|-1.88283833840455|1.21361952639238>>>|<with|color|dark
  blue|fill-color|pastel orange|<cspline|<point|-0.0201581|1.78512>|<point|-0.845664109009128|1.36178727344887>|<point|-0.464661330863871|0.493947612118005>|<point|0.424345151475063|0.917284032279402>>>|<with|color|dark
  magenta|fill-color|pastel red|<cspline|<point|2.11769|1.99679>|<point|2.81619592538696|0.811449927239053>|<point|1.56735348591083|0.769116285222913>>>>>>
    Examples of a few closed splines with different colors and fill colors.
  </big-figure>

  <paragraph|Opacity>This property also applies to any of the graphical
  object types and specifies an opacity between 0% and 100%. The default is
  100% and lower opacities will make the object more transparent.

  <big-figure|<with|gr-mode|<tuple|edit|carc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-fill-color|pastel
  green|gr-color|dark green|gr-opacity|20%|gr-auto-crop|true|<graphics||<with|color|dark
  brown|fill-color|brown|<cline|<point|-5|1>|<point|-5.0|-1.0>|<point|5.0|-1.0>|<point|5.0|1.0>>>|<with|color|dark
  green|fill-color|pastel green|<carc|<point|4|-1.5>|<point|4.5|-1.0>|<point|4.0|-0.5>>>|<with|color|dark
  green|opacity|80%|fill-color|pastel green|<carc|<point|2|-1.5>|<point|2.5|-1.0>|<point|2.0|-0.5>>>|<with|color|dark
  green|opacity|60%|fill-color|pastel green|<carc|<point|0|-1.5>|<point|0.5|-1.0>|<point|0.0|-0.5>>>|<with|color|dark
  green|opacity|40%|fill-color|pastel green|<carc|<point|-2|-1.5>|<point|-1.5|-1.0>|<point|-2.0|-0.5>>>|<with|color|dark
  green|opacity|20%|fill-color|pastel green|<carc|<point|-4|-1.5>|<point|-3.5|-1.0>|<point|-4.0|-0.5>>>>>|Examples
  of the same object with increasing opacities on top of another object.>

  <paragraph|Point style>A few different point styles are supported for
  displaying points: solid disks, round circles and squares.

  <big-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-fill-color|red|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-point-style|square|gr-text-at-halign|center|gr-auto-crop|true|<graphics||<with|fill-color|red|<point|-2|0>>|<with|fill-color|red|point-style|round|<point|0|0>>|<with|fill-color|red|point-style|square|<point|2|0>>|<with|text-at-halign|center|<text-at|Square|<point|2|0.5>>>|<with|text-at-halign|center|<text-at|Round|<point|0|0.5>>>|<with|text-at-halign|center|<text-at|Disk|<point|-2|0.5>>>>>|The
  different point styles for black color and red fill color.>

  <paragraph|Line width>The line width property applies to all curves (that
  is, to broken lines, polygons, splines, closed splines, arcs and circles).
  By default it is <verbatim|1ln>, the width of the fraction bar in
  mathematical formulas, but any <TeXmacs> length unit can be used instead.

  <\big-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|5ln|gr-auto-crop|true|<graphics||<spline|<point|-3|0>|<point|-2.0|0.3>|<point|0.0|-0.3>|<point|2.0|0.3>|<point|3.0|0.0>>|<with|line-width|0.5ln|<spline|<point|-3|1>|<point|-2.0|1.3>|<point|0.0|0.7>|<point|2.0|1.3>|<point|3.0|1.0>>>|<with|line-width|2ln|<spline|<point|-3|-1>|<point|-2.0|-0.7>|<point|0.0|-1.3>|<point|2.0|-0.7>|<point|3.0|-1.0>>>|<with|line-width|5ln|<spline|<point|-3|-2>|<point|-2.0|-1.7>|<point|0.0|-2.3>|<point|2.0|-1.7>|<point|3.0|-2.0>>>|<text-at|0.5ln|<point|3.5|1>>|<text-at|1ln|<point|3.5|0>>|<text-at|2ln|<point|3.5|-1>>|<text-at|5ln|<point|3.5|-2>>>>>
    The same curve using different line widths.
  </big-figure>

  <paragraph|Line dashes>Various dash styles are available for curves in
  <menu|Focus|Line dashes>.

  <\big-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|2ln|gr-dash-style|11100|gr-dash-style-unit|20ln|gr-auto-crop|true|<graphics||<with|line-width|2ln|<line|<point|-3|1>|<point|-2.0|1.5>|<point|0.0|0.5>|<point|2.0|1.5>|<point|3.0|1.0>>>|<with|dash-style|10|line-width|2ln|<line|<point|-3|0.5>|<point|-2.0|1.0>|<point|0.0|0.0>|<point|2.0|1.0>|<point|3.0|0.5>>>|<with|dash-style|11100|line-width|2ln|<line|<point|-3|0>|<point|-2.0|0.5>|<point|0.0|-0.5>|<point|2.0|0.5>|<point|3.0|0.0>>>|<with|dash-style|1111010|line-width|2ln|<line|<point|-3|-0.5>|<point|-2.0|0.0>|<point|0.0|-1.0>|<point|2.0|0.0>|<point|3.0|-0.5>>>|<with|dash-style|1111010|line-width|2ln|dash-style-unit|10ln|<line|<point|-3|-1>|<point|-2.0|-0.5>|<point|0.0|-1.5>|<point|2.0|-0.5>|<point|3.0|-1.0>>>|<with|dash-style|11100|line-width|2ln|dash-style-unit|20ln|<line|<point|-3|-1.5>|<point|-2.0|-1.0>|<point|0.0|-2.0>|<point|2.0|-1.0>|<point|3.0|-1.5>>>>>>
    The same curve using different dashing styles.
  </big-figure>

  <paragraph|Line arrows>Various arrows at the ends of curves are supported
  in <menu|Focus|Line arrows>.

  <\big-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|2ln|gr-arrow-begin|\||gr-arrow-end|\||gr-auto-crop|true|<graphics||<with|line-width|2ln|<line|<point|-4|2>|<point|-1.0|2.0>>>|<with|arrow-end|\<gtr\>|line-width|2ln|<line|<point|-4|1.5>|<point|-1.0|1.5>>>|<with|arrow-end|\|\<gtr\>|line-width|2ln|<line|<point|-4|1>|<point|-1.0|1.0>>>|<with|arrow-end|\<gtr\>\<gtr\>|line-width|2ln|<line|<point|-4|0.5>|<point|-1.0|0.5>>>|<with|arrow-end|\<less\>|line-width|2ln|<line|<point|-4|0>|<point|-1.0|0.0>>>|<with|arrow-end|\<less\>\||line-width|2ln|<line|<point|-4|-0.5>|<point|-1.0|-0.5>>>|<with|arrow-end|\<less\>\<less\>|line-width|2ln|<line|<point|-4|-1>|<point|-1.0|-1.0>>>|<with|arrow-end|\||line-width|2ln|<line|<point|-4|-1.5>|<point|-1.0|-1.5>>>|<with|arrow-end|o|line-width|2ln|<line|<point|-4|-2>|<point|-1.0|-2.0>>>|<with|arrow-end|\|\<gtr\>|line-width|2ln|arrow-begin|\<less\>\||<line|<point|0|2>|<point|3.0|2.0>>>|<with|arrow-end|\|\<gtr\>|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|1.5>|<point|3.0|1.5>>>|<with|arrow-end|o|line-width|2ln|arrow-begin|o|<line|<point|0|1>|<point|3.0|1.0>>>|<with|arrow-end|\<less\>\||line-width|2ln|arrow-begin|\|\<gtr\>|<line|<point|0|0.5>|<point|3.0|0.5>>>|<with|arrow-end|\<less\>|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|0>|<point|3.0|0.0>>>|<with|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|-0.5>|<point|3.0|-0.5>>>|<with|arrow-end|\<gtr\>\<gtr\>|line-width|2ln|arrow-begin|\<gtr\>|<line|<point|0|-1>|<point|3.0|-1.0>>>|<with|arrow-end|\<gtr\>\<gtr\>|line-width|2ln|arrow-begin|\<gtr\>\<gtr\>|<line|<point|0|-1.5>|<point|3.0|-1.5>>>|<with|arrow-end|\||line-width|2ln|arrow-begin|\||<line|<point|0|-2>|<point|3.0|-2.0>>>>>>
    The same segment using different types of arrows at the extremities.
  </big-figure>

  <paragraph|Text alignment>For textual and mathematical boxes, its is
  possible to specify the horizontal and vertical alignment properties, as
  indicated in the figure below:

  <\big-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.420008gw|0.5gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-line-width|2ln|gr-text-at-halign|center|gr-text-at-valign|top|gr-auto-crop|true|<graphics||<with|color|light
  grey|line-width|2ln|<line|<point|-3|2>|<point|-3.0|-2.0>>>|<with|text-at-valign|center|<text-at|Left|<point|-3|1>>>|<with|text-at-valign|center|text-at-halign|center|<text-at|Center|<point|-3|0>>>|<with|text-at-valign|center|text-at-halign|right|<text-at|Right|<point|-3|-1>>>|<with|color|light
  grey|line-width|2ln|<line|<point|-1|0>|<point|7.0|0.0>>>|<with|text-at-valign|bottom|text-at-halign|center|<text-at|Bottom
  <math|y>|<point|0|0>>>|<with|text-at-halign|center|<text-at|Base
  <math|y>|<point|1.5|0>>>|<with|text-at-valign|axis|text-at-halign|center|<text-at|Axis
  <math|y>|<point|3|0>>>|<with|text-at-valign|center|text-at-halign|center|<text-at|Center
  <math|y>|<point|4.5|0>>>|<with|text-at-valign|top|text-at-halign|center|<text-at|Top
  <math|y>|<point|6|0>>>>>>
    Illustration of horizontal and vertical alignment of text boxes.
  </big-figure>

  \;

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>