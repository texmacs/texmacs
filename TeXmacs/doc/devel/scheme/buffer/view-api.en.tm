<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Manipulating <TeXmacs> views>

  <\explain>
    <scm|(view-list)><explain-synopsis|list of all views>
  <|explain>
    This routine returns the list of all available views, sorted by inverse
    chronological order. That is, views which were selected more recently
    will occur earlier in the list.
  </explain>

  <\explain>
    <scm|(current-view)><explain-synopsis|current view>
  <|explain>
    Return the current view or <scm|#f>.
  </explain>

  <\explain>
    <scm|(view-\<gtr\>buffer <scm-arg|vw>)><explain-synopsis|buffer to which
    the view is attached>
  <|explain>
    This routine returns the buffer to which the view <scm-arg|vw> is
    attached.
  </explain>

  <\explain>
    <scm|(view-\<gtr\>window <scm-arg|vw>)><explain-synopsis|window to which
    the view is attached>
  <|explain>
    This routine returns the window in which the view <scm-arg|vw> is being
    displayed or <scm|#f>.
  </explain>

  <\explain>
    <scm|(view-new <scm-arg|buf>)>

    <scm|(view-passive <scm-arg|buf>)>

    <scm|(view-recent <scm-arg|buf>)><explain-synopsis|get view on buffer>
  <|explain>
    All three routines return a view on the buffer <scm-arg|buf>. In the case
    of <scm|view-new>, we systematically create a new view. The routine
    <scm|view-passive> first attempts to find an existing view on
    <scm-arg|buf> which is not attached to a window; if no such view exists,
    then a new one is created. The last routine <scm|view-recent> returns the
    most recent existing view, with a preference for the current view, or
    another visible view. Again, a new view is created if no suitable recent
    view exists.
  </explain>

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