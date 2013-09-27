<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Overlays>

  The standard ``fold'', ``unroll'' and ``switch'' tags implement the most
  frequent kinds of traversal of a slideshow. However, there are cases in
  which more complex successions are needed.

  <\big-figure|<block|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<twith|table-width|0.333par>|<twith|table-hmode|exact>|<table|<row|<\cell>
    <\compact>
      Elimination of <math|x> from

      <\equation*>
        x*sin y-3*x*y=\<alpha\>
      </equation*>

      yields

      <\equation*>
        x=<frac|\<alpha\>|sin y-3*y>.
      </equation*>
    </compact>
  </cell>>>>><space|2em><block|<tformat|<cwith|1|1|1|1|cell-hyphen|t>|<twith|table-width|0.333par>|<twith|table-hmode|exact>|<table|<row|<\cell>
    <\compact>
      Elimination of <math|<with|color|red|x>> from

      <\equation*>
        <with|color|red|x>*sin y-3*<with|color|red|x>*y=\<alpha\>
      </equation*>

      yields

      <\equation*>
        <with|color|red|x>=<frac|\<alpha\>|sin y-3*y>.
      </equation*>
    </compact>
  </cell>>>>>>
    <label|red-x-fig>Example of highlighting a variable <math|x> when
    switching from one slide to a next one.
  </big-figure>

  For instance, imagine that we are given a slide, and that we wish to
  highlight all occurrences of some variable <math|x> in red on the next
  slide (see figure<nbsp><reference|red-x-fig>). This could be achieved by
  using a switch tag: we just copy the whole slide both in the first and in
  the second branch of the switch, and next color all instances of <math|x>
  red in the second branch. However, this solution has the disadvantage that
  any <em|a posteriori> modification on the slide has to be made both in the
  first and in the second branch.

  <TeXmacs> provides a so called ``overlay'' mechanism for this kind of more
  complex successions of slides. You may insert a pile of overlays using
  <menu|Insert|Fold|Overlays|Standard>. At the start, the pile contains a
  unique overlay, but new overlays can then be added using the standard
  keyboard shortcuts <shortcut|(structured-insert-right)> and
  <shortcut|(structured-insert-left)> for <hlink|structured
  insertion|../editing/man-structured-editing.en.tm>. When applied to
  overlays, the standard keys <key|F10> and <key|F11> for traversing the
  presentation have the effect of going up and down in the pile of overlays.

  By default, all text which is typed by the user will be visible on all
  overlays. But, using the filters in the menu <menu|Insert|Fold|Overlay>, it
  is also possible to create text which is only visible on specified overlays
  of the pile. There are four basic types of filters:

  <\description>
    <item*|<subsubsubmenu|Insert|Fold|Overlay|Visible from here on>>Text that
    will be visible on this and all subsequent overlays.

    <item*|<subsubsubmenu|Insert|Fold|Overlay|Visible until here>>Text that
    will be visible on this and all previous overlays.

    <item*|<subsubsubmenu|Insert|Fold|Overlay|Visible only here>>Text that
    will be visible only on this overlay.

    <item*|<subsubsubmenu|Insert|Fold|Overlay|Visible except here>>Text that
    will be visible on all but the current overlays.
  </description>

  <tmdoc-copyright|2013|Joris van der Hoeven>

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