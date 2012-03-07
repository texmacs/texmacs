<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Editing objects>

  Any of the modes which allows for the insertion of new objects (points,
  lines, polygons, etc.) also allows you to directly edit existing objects.
  More precisely, as soon as you go over an existing object with your mouse,
  then the control points for that object will be highlighted automatically.
  Several editing operations are supported:

  <\description>
    <item*|Moving control points>When your mouse is sufficiently close to a
    control point, then it will be possible to drag and drop the control
    point to somewhere else using the left mouse button.

    <item*|Inserting new control points>For objects with an arbitrary number
    of control points, such as broken lines, polygons, splines and closed
    splines, it is possible to insert new points on existing edges. In order
    to do so, move the mouse pointer on the edge where you want to insert a
    new point; the two neighbouring control points should be highlighted.
    Then insert a new point drag and move it around using drag and drop for
    the first mouse button.

    <item*|Removing control points>Using the middle mouse button, it is
    possible to remove control points (and eventually the object itself).

    <item*|Removing the entire object>Using the middle mouse button while
    simultaneously pressing the shift key <key|shift> removes the entire
    object which is currently highlighted.
  </description>

  While editing, it should also be noticed that <TeXmacs> attempts to
  automatically <em|snap> the mouse pointer to control points or edges of
  existing objects, as well as to intersection points of two curves and
  points on the grid. This makes it possible to quickly draw complex pictures
  which are exact, and not merely exact up to one or more pixels (and ugly
  when magnified or printed). Around boxes with text or mathematical
  formulas, there are also eight invisible control points to which <TeXmacs>
  will attempt to snap the mouse pointer. This makes it easier to draw
  diagrams as in figure<nbsp><reference|diagram-fig> below.

  Graphical objects are drawn in a specific <em|stacking order> which has the
  effect that certain objects may be hidden by other objects. Using
  <shortcut|(graphics-zmove 'closer)> and <shortcut|(graphics-zmove
  'farther)>, you may move the currently highlighted object closer to or
  farther away from the eye for this stacking order. In a similar vein,
  certain control points may become unaccessible, because hidden by closer
  control points. In that case, you may use <key|tab> to cycle through all
  possibilities for the current cursor position.

  <\big-figure|<with|gr-mode|<tuple|edit|spline>|gr-frame|<tuple|scale|1cm|<tuple|0.729978gw|0.140033gh>>|gr-geometry|<tuple|geometry|1par|0.6par>|gr-grid|<tuple|empty>|gr-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|10|none>>|gr-edit-grid|<tuple|empty>|gr-edit-grid-old|<tuple|cartesian|<point|0|0>|1>|gr-text-at-halign|center|gr-arrow-end|\<gtr\>|gr-auto-crop|true|<graphics||<with|text-at-halign|center|<math-at|A|<point|-5|3>>>|<with|text-at-halign|center|<math-at|B|<point|-3.5|3>>>|<with|text-at-halign|center|<math-at|C|<point|-5|2>>>|<with|text-at-halign|center|<math-at|D|<point|-3.5|2>>>|<with|text-at-halign|center|<math-at|X|<point|-2|3>>>|<with|text-at-halign|center|<math-at|Y|<point|-2|2>>>|<with|arrow-end|\<gtr\>|<line|<point|-4.75015|3.12052>|<point|-3.75133946289192|3.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-5|2.88244>|<point|-5.0|2.35861225029766>>>|<with|arrow-end|\<gtr\>|<line|<point|-4.75637|2.12052>|<point|-3.76357653128721|2.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-3.5|2.88244>|<point|-3.5|2.35861225029766>>>|<with|arrow-end|\<gtr\>|<line|<point|-3.24864|3.12052>|<point|-2.26367575076068|3.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-3.23641|2.12052>|<point|-2.21995303611589|2.1205185871147>>>|<with|arrow-end|\<gtr\>|<line|<point|-2|2.88244>|<point|-2.0|2.35861225029766>>>|<with|arrow-end|\<gtr\>|<spline|<point|-4.75015|3.35861>|<point|-2.87767892578383|3.71332186797195>|<point|-1.31133417118666|3.26881862680249>|<point|-1.7800304273052|2.35861225029766>>>>>>
    <label|diagram-fig>Example of a diagram which was drawn by using snapping
    to the eight control points around each box with a mathematical formula.
    Notice also that we cropped the graphics to its actual size.
  </big-figure>

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>