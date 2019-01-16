<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|<scheme> serialization>

  Users may write their own extensions to <TeXmacs> in the <scheme> extension
  language. In that context, <TeXmacs> trees are usually represented by
  <scheme> expressions. The <scheme> syntax was designed to be predictable,
  easy to hand-edit, and expose the complete internal structure of the
  document. For instance, the formula (<reference|tm-tree-ex>) is represented
  by

  <\tm-fragment>
    <verbatim|(with "mode" "math" (concat "x+y+" (frac "1" "2") "+" (sqrt
    "y+z")))>
  </tm-fragment>

  The <scheme> representation may also be useful in order to represent
  complex macros with a lot of programatic content. Finally, <scheme> is the
  safest format when incorporating <TeXmacs> snippets into emails. Indeed,
  both the standard <TeXmacs> format and the XML serialization may be quite
  sensitive to white-space.

  In order to save or load a document in <scheme> format, you may use
  <menu|File|Export|Scheme> <abbr|resp.> <menu|File|Import|Scheme>. Files
  saved in <scheme> format can easily be processed by external <scheme>
  programs, in the same way as files saved in XML format can easily be
  processed by tools for processing XML, like XSLT.

  In order to copy a document fragment to an email in <scheme> format, you
  may use <menu|Edit|Copy to|Scheme>. Similarly, you may paste external
  <scheme> fragments into <TeXmacs> using <menu|Edit|Paste from|Scheme>. The
  <scheme> format may also used interactively inside <scheme> sessions or
  interactive commands. For instance, typing <shortcut|(interactive
  exec-interactive-command)> followed by the interactive command

  <\scm-code>
    (insert '(frac "1" "2"))
  </scm-code>

  inserts the fraction <frac|1|2> at the current cursor position.

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>