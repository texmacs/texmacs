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

  <\explain>
    <scm|(buffer-set <scm-arg|buf> <scm-arg|rich-t>)>

    <scm|(buffer-get <scm-arg|buf>)><explain-synopsis|set/get the contents of
    the buffer>
  <|explain>
    Set the contents of the buffer <scm-arg|buf> to the rich tree
    <scm-arg|rich-t>, <abbr|resp.> get the rich contents of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-set-body <scm-arg|buf> <scm-arg|t>)>

    <scm|(buffer-get-body <scm-arg|buf>)><explain-synopsis|set/get the main
    body of the buffer>
  <|explain>
    Set the main body of the buffer <scm-arg|buf> to the tree <scm-arg|t>,
    <abbr|resp.> get the main body of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-set-master <scm-arg|buf> <scm-arg|master>)>

    <scm|(buffer-get-master <scm-arg|buf>)><explain-synopsis|set/get the
    master of the buffer>
  <|explain>
    Set the master of the buffer <scm-arg|buf> to <scm-arg|master>,
    <abbr|resp.> get the master of <scm-arg|buf>. The master of a buffer
    should again be a buffer. Usually, the master of a buffer is the buffer
    itself. Otherwise, the buffer will behave similarly as its master in some
    respects. For instance, if a buffer <verbatim|a/b.tm> admits
    <verbatim|x/y.tm> as its master, then a hyperlink to <verbatim|c.tm> will
    point to <verbatim|x/c.tm> and not to <verbatim|a/c.tm>.
  </explain>

  <\explain>
    <scm|(buffer-set-title <scm-arg|buf> <scm-arg|name>)>

    <scm|(buffer-get-title <scm-arg|buf>)><explain-synopsis|set/get the title
    of the buffer>
  <|explain>
    Set the title of the buffer <scm-arg|buf> to the string <scm-arg|name>,
    <abbr|resp.> get the title of <scm-arg|buf>. The title is for instance
    used as the title for the window.
  </explain>

  <\explain>
    <scm|(buffer-set-title <scm-arg|buf> <scm-arg|name>)>

    <scm|(buffer-get-title <scm-arg|buf>)><explain-synopsis|set/get the title
    of the buffer>
  <|explain>
    Set the title of the buffer <scm-arg|buf> to the string <scm-arg|name>,
    <abbr|resp.> get the title of <scm-arg|buf>. The title is for instance
    used as the title for the window.
  </explain>

  <\explain>
    <scm|(buffer-last-save <scm-arg|buf>)>

    <scm|(buffer-last-visited <scm-arg|buf>)><explain-synopsis|time when a
    buffer was visited/saved last>
  <|explain>
    Return the time when the buffer <scm-arg|buf> was visited or saved last.
  </explain>

  <\explain>
    <scm|(buffer-modified? <scm-arg|buf>)>

    <scm|(buffer-pretend-saved <scm-arg|buf>)><explain-synopsis|check for
    modifications since last save>
  <|explain>
    The predicate <scm|buffer-modified?> check whether the buffer
    <scm-arg|buf> was modified since the last time it was saved. The routine
    <scm|buffer-pretend-saved> can be used in order to pretend that
    the<nbsp>buffer <scm-arg|buf> was saved, without actually saving it. This
    can for instance be useful if no worthwhile changes occurred in the
    buffer since the genuine last save.
  </explain>

  <paragraph|Synchronizing with the external world>

  Buffers inside <TeXmacs> usually correspond to actual files on disk or
  elsewhere. When changes occur on either side (<abbr|e.g.> when editing the
  buffer, or modifying the file on disk using an external program), the
  following routines can be used in order to synchronize the buffer inside
  <TeXmacs> with its corresponding file on disk.

  <\explain>
    <scm|(buffer-load <scm-arg|buf>)><explain-synopsis|load buffer>
  <|explain>
    Retrieve the buffer <scm-arg|buf> from disk (or elsewhere). Returns
    <scm|#t> on error and <scm|#f> otherwise. The format being used for
    loading files is chosen as a function of the extension of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-save <scm-arg|buf>)><explain-synopsis|save buffer>
  <|explain>
    Save the buffer <scm-arg|buf> to disk (or elsewhere). Returns <scm|#t> on
    error and <scm|#f> otherwise. The format being used for saving files is
    chosen as a function of the extension of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-import <scm-arg|buf> <scm-arg|src>
    <scm-arg|fm>)><explain-synopsis|import buffer>
  <|explain>
    Import the buffer <scm-arg|buf> from <scm-arg|src>, using the format
    <scm-arg|fm>. Returns <scm|#t> on error and <scm|#f> otherwise.
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