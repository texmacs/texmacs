<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Graphics interface between C++ and <scheme>>

  <paragraph*|Rationale>

  <TeXmacs> both implements a low-level part of the graphics in C++ and the
  high-level user interface in <scheme>. This API describes how both parts
  interact.

  The low-level C++ mainly takes care of transforming the graphical markup in
  a typeset box. It also provides routines for translating between physical
  coordinates (relative to the window) into logical coordinates (the local
  coordinate system of the graphics) and routines for interacting with the
  typeset boxes (finding the closest objects to a given point or region or
  projecting a point on a grid).

  <paragraph*|Definitions>

  <\description>
    <item*|Editor coordinates>The coordinates of the outermost typeset box.
    Mouse events are typically passed in these coordinates. The corresponding
    data type is <verbatim|SI>.

    <item*|Graphics coordinates>The coordinates of the innermost graphics
    corresponding to the current cursor position.

    <item*|Grid>The current grid relative to the graphics for editing objects
    (this grid may theoretically be different from the grid which is
    displayed). The current grid consists both of a mathematical type of grid
    (no grid, cartesian grid, polar grid, etc.), together with special points
    which correspond either to control points, intersections of curves with
    the grid, intersections of curves, or self-intersections of curves.

    <item*|Grid point>A point on the grid is a triple <scm|(<scm-arg|p>
    <scm-arg|distance> <scm-arg|type>)>, where <scm-arg|p> is a point in
    graphics coordinates, <scm-arg|distance> its distance to the point which
    was projected on the grid (see <verbatim|grid-project> below) and
    <scm-arg|type> the type of grid point with a potential origin. For
    instance, <scm-arg|type> can be <verbatim|plain> or something like
    <verbatim|(control t)> for a control point corresponding to the tree
    <scm|t> in the document.
  </description>

  <paragraph*|Coordinate transformations>

  <\explain>
    <scm|(editor-\<gtr\>graphics <scm-arg|p>)><explain-synopsis|get graphics
    coordinates>
  <|explain>
    Transform a point <scm-arg|p> of the form <scm|(<scm-arg|x> <scm-arg|y>)>
    from the editor coordinates into the graphics coordinates.
  </explain>

  <\explain>
    <scm|(graphics-\<gtr\>editor p)><explain-synopsis|get editor coordinates>
  <|explain>
    Transform a point <scm-arg|p> of the form <scm|(<scm-arg|x> <scm-arg|y>)>
    from the graphics coordinates into the editor coordinates.
  </explain>

  <paragraph*|Grid routines>

  <\explain>
    <scm|(grid-project <scm-arg|p>)><explain-synopsis|project point on grid>
  <|explain>
    Given a point <scm-arg|p> (in graphics coordinates), find its projection
    on \ the current grid, the <scm-arg|distance> part of the projection
    being the distance between <scm-arg|p> and its projection.

    Note: the routine grid-project can also be used in order to find editable
    shapes and groups close to the current pointer position. Indeed, the
    corresponding control points are understood to lie on the grid in our
    sense.
  </explain>

  <\explain>
    <scm|(grid-point-pertinence\<less\>? <scm-arg|p> <scm-arg|q>)>

    <scm|(grid-point-pertinence\<less\>=? <scm-arg|p>
    <scm-arg|q>)><explain-synopsis|order by pertinence>
  <|explain>
    Grid points are ordered by pertinence as a function of type and distance.
    For instance, control points have higher pertinence than plain grid
    points and closer grid points are considered better than farther ones.
  </explain>

  <paragraph*|Selection of shapes>

  <\explain>
    <scm|(graphics-find-disk <scm-arg|p> <scm-arg|r>)><explain-synopsis|search
    shapes in disk>
  <|explain>
    Return the list of all trees in the graphics which intersect a disk with
    center <scm-arg|p> and radius <scm-arg|r> (in graphics coordinates).
  </explain>

  <\explain>
    <scm|(graphics-find-rectangle <scm-arg|p>
    <scm-arg|q>)><explain-synopsis|search shapes in rectangle>
  <|explain>
    Return the list of all trees in the graphics which intersect a rectangle
    with corners <scm-arg|p> and <scm-arg|q> (in graphics coordinates).
  </explain>

  <paragraph*|Computations with shapes>

  <\explain>
    <scm|(box-info t)><explain-synopsis|get bounding box for a shape>
  <|explain>
    \ Get a bounding box (and other information) about a shape <scm-arg|t>.
    <scm-arg|t> can be a tree or a scheme tree.
  </explain>

  <\remark>
    This section might be extended, since a lot of the graphical intelligence
    is implemented in the C++ code. For instance, we might want to compute
    the intersections of two curves inside the Scheme code. Also, when we
    will allow for user macros, we might want routines which return the
    graphical expansion of the macro (the constituent elementary shapes, i.e.
    polylines, splines, etc.).
  </remark>

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>