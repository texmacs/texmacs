<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Starting a new picture>

  You may start drawing a new picture using <menu|Insert|Image|Draw image>.
  In some cases, you may also want to draw something on top of an existing
  image (or other kinds of content). This can be done by selecting the image
  or content on top of which you want to draw, and then click on
  <menu|Insert|Image|Draw over selection>.

  By default, the inserted image spans over the whole paragraph. You may
  adjust its size using the keyboard shortcuts
  <shortcut|(graphics-decrease-hsize)>, <shortcut|(graphics-increase-hsize)>,
  <shortcut|(graphics-decrease-vsize)>, <shortcut|(graphics-increase-vsize)>
  (to adjust the size a bit faster, you may use
  <shortcut|(graphics-decrease-hsize-fast)>,
  <shortcut|(graphics-increase-hsize-fast)>,
  <shortcut|(graphics-decrease-vsize-fast)>,
  <shortcut|(graphics-increase-vsize-fast)>). You may also specify an
  explicit size using <menu|Insert|Geometry|Size>. After completion of your
  drawing, you may automatically crop the size of your picture to its actual
  size (plus some additional padding), using <menu|Insert|Geometry|Crop>.

  For technical pictures, it is often useful to display a grid while you are
  drawing. This can be done using<nbsp><menu|Insert|Grid|Type|Cartesian>. In
  the menu <menu|Insert|Grid> it is also possible to adjust the colors of the
  axes and the grid-lines, as well as the number of subunit grid-lines per
  unit grid-line. By default, grids will also be printed; you need to remove
  them after completing your drawing if you do not want this.

  By default, <TeXmacs> places the origin of the grid at the center of the
  screen and uses a <verbatim|1cm> unit. You may scroll the picture using the
  arrow keys <shortcut|(graphics-move-origin-left)>,
  <shortcut|(graphics-move-origin-right)>,
  <shortcut|(graphics-move-origin-up)>, <shortcut|(graphics-move-origin-down)>
  (or <shortcut|(graphics-move-origin-left-fast)>,
  <shortcut|(graphics-move-origin-right-fast)>,
  <shortcut|(graphics-move-origin-up-fast)>,
  <shortcut|(graphics-move-origin-down-fast)> if you want to move fast). You
  may specify a different unit using the <menu|Insert|Geometry|Unit> menu.
  You may also zoom in and out using <shortcut|(graphics-zoom-in)> and
  <shortcut|(graphics-zoom-out)>, or from the <menu|Insert|Geometry|Zoom>
  menu.

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