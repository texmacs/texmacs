<TeXmacs|1.99.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Positioning and resizing objects>

  The <prefix|structured:geometry> prefix may be used for positioning and
  resizing objects. For instance, inside a cell of a<nbsp>table, you may use
  <key|structured:geometry right> to align the cell more to the right. Behind
  a space introduced <em|via> <menu|Format|Space>, the same key allows you to
  increase the width of space. More generally, the following shortcuts are
  implemented:

  <\description>
    <item*|<shortcut|(geometry-left)>>Decrease the horizontal size of an
    object, or align more to the left.

    <item*|<shortcut|(geometry-right)>>Increase the horizontal size of an
    object, or align more to the right.

    <item*|<shortcut|(geometry-down)>>Decrease/increase the vertical size of
    an object, or align more to the bottom.

    <item*|<shortcut|(geometry-up)>>Increase/decrease the vertical size of an
    object, or align more to the top.

    <item*|<shortcut|(geometry-start)>>Decrease the horizontal offset of an
    object, or left align.

    <item*|<shortcut|(geometry-end)>>Increase the horizontal offset of an
    object, or right align.

    <item*|<shortcut|(geometry-bottom)>>Decrease the vertical offset of an
    object, or align at the bottom.

    <item*|<shortcut|(geometry-top)>>Increase the vertical offset of an
    object, or align at the top.

    <item*|<shortcut|(geometry-reset)>>Revert the geometry (size, position,
    alignment) to the defaults.

    <item*|<shortcut|(geometry-circulate #t)>, <shortcut|(geometry-circulate
    #f)>>Circulate among the available length units for specifying the
    geometry.

    <item*|<shortcut|(geometry-slower)>, <shortcut|(geometry-faster)>>Decrease
    or increase the step size when positioning or resizing.
  </description>

  Particular tags to which the shortcuts apply are the following:

  <\description>
    <item*|Spaces>Both horizontal and vertical spaces from the
    <menu|Format|Space> menu. You should put the cursor just after the space
    tag for the shortcuts to apply.

    <item*|Box modifiers>The tags <markup|move>, <markup|shift>,
    <markup|resize>, <markup|extend>, <markup|clipped>, <markup|smashed>,
    <markup|inflate> from the <menu|Format|Adjust> menu.

    <item*|Animations>The durations of animations can be modified using
    <shortcut|(geometry-left)> and <shortcut|(geometry-right)>.

    <item*|Images>The size and alignment of images can be changed.
  </description>

  <tmdoc-copyright|1998--2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>