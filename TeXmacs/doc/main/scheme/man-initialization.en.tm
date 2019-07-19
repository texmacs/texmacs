<TeXmacs|1.99.10>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Writing your own initialization files>

  When starting up, <TeXmacs> executes the file

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/init-texmacs.scm
  </verbatim>

  as well as your personal initialization file

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-texmacs.scm
  </verbatim>

  if it exists. By default, the path <verbatim|$TEXMACS_HOME_PATH> equals
  <verbatim|%appdata%\\TeXmacs> on <name|Windows> or
  <verbatim|$HOME/.TeXmacs> on <name|GNU>/<name|Linux> and <name|macOS>.
  Similarly, each time you create a new buffer (either by creating a new file
  or opening an already existing one), the file

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/init-buffer.scm
  </verbatim>

  is executed, as well as

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-buffer.scm
  </verbatim>

  if it exists.

  <\example>
    Suppose you want to add a style package <verbatim|CustomStyle.ts> of your
    own to every new document you create. You can add the following lines to
    <verbatim|$TEXMACS_HOME_PATH/progs/my-init-buffer.scm>:

    <\scm-code>
      (when (buffer-newly-created? (current-buffer))

      \ \ (set-style-list (append (get-style-list) '("CustomStyle")))

      \ \ (buffer-pretend-saved (current-buffer)))
    </scm-code>

    First we check whether the <scm|current-buffer> has been newly created in
    order not to apply the style to existing files when we open them. Then we
    add the new package (instead of changing it with <scm|init-style>) using
    <scm|set-style-list> and finally we call <scm|buffer-pretend-saved> to
    prevent <TeXmacs> from thinking the buffer has been modified by the
    change of style, or it would always prompt asking for confirmation before
    closing an empty buffer.
  </example>

  <tmdoc-copyright|1998--2019|Joris van der Hoeven|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>