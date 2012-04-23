<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|Manipulating <TeXmacs> buffers>

  <paragraph|Basic buffer management>

  <\explain>
    <scm|(buffer-list)><explain-synopsis|list of all buffers>
  <|explain>
    This routine returns the list of all open buffers.
  </explain>

  <\explain>
    <scm|(current-buffer)><explain-synopsis|current buffer>
  <|explain>
    Return the current view. The program may abort if there exists no current
    buffer.
  </explain>

  <\explain>
    <scm|(path-\<gtr\>buffer <scm-arg|p>)><explain-synopsis|buffer which
    contains a certain path>
  <|explain>
    Return the buffer which contains a certain path <scm-arg|p>, or <scm|#f>.
  </explain>

  <\explain>
    <scm|(tree-\<gtr\>buffer <scm-arg|t>)><explain-synopsis|buffer which
    contains a certain tree>
  <|explain>
    Return the buffer which contains a certain tree <scm-arg|t>, or <scm|#f>.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>views <scm-arg|buf>)><explain-synopsis|list of views
    on a buffer>
  <|explain>
    This routine returns the list of views on the buffer <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>windows <scm-arg|buf>)><explain-synopsis|list of
    windows containing buffer>
  <|explain>
    This routine returns the list of windows in which the buffer
    <scm-arg|buf> is currently being displayed.
  </explain>

  <\explain>
    <scm|(buffer-new)><explain-synopsis|create a new buffer>
  <|explain>
    Create a new buffer and returns its URL.
  </explain>

  <\explain>
    <scm|(buffer-rename <scm-arg|buf> <scm-arg|new-name>)><explain-synopsis|create
    a new buffer>
  <|explain>
    Give a new name <scm-arg|new-name> to the buffer <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(switch-to-buffer <scm-arg|buf>)><explain-synopsis|switch the
    editor's focus>
  <|explain>
    Switch the editor's focus to the buffer <scm-arg|buf>.
  </explain>

  <paragraph|Information associated to buffers>

  <paragraph|Synchronizing with the external world>

  <\explain>
    <scm|(buffer-load <scm-arg|buf>)><explain-synopsis|load buffer>
  <|explain>
    Retrieve the buffer <scm-arg|buf> from disk (or elsewhere). Returns
    <scm|#t> on error and <scm|#f> otherwise.
  </explain>

  <\explain>
    <scm|(buffer-save <scm-arg|buf>)><explain-synopsis|save buffer>
  <|explain>
    Save the buffer <scm-arg|buf> to disk (or elsewhere). Returns <scm|#t> on
    error and <scm|#f> otherwise.
  </explain>

  <\explain>
    <scm|(buffer-export <scm-arg|buf> <scm-arg|dest>
    <scm-arg|fm>)><explain-synopsis|export buffer>
  <|explain>
    Export the buffer <scm-arg|buf> to <scm-arg|dest>, using the format
    <scm-arg|fm>. Returns <scm|#t> on error and <scm|#f> otherwise.
  </explain>

  <\explain>
    <scm|(tree-import <scm-arg|src> <scm-arg|fm>)><explain-synopsis|import a
    tree>
  <|explain>
    Import a tree from the URL <scm-arg|src>, using the format <scm-arg|fm>.
  </explain>

  <\explain>
    <scm|(tree-export <scm-arg|t> <scm-arg|dest>
    <scm-arg|fm>)><explain-synopsis|export a tree>
  <|explain>
    Export a tree to the URL <scm-arg|dest>, using the format <scm-arg|fm>.
  </explain>

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>