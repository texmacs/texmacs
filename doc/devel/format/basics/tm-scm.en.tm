<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|<value|scheme> serialization>

  In contexts where a document cannot be represented and edited in its
  typeset form, like in <value|scheme> programs or in e-mails, or when the
  important information is the internal representation, the preferred syntax
  is <value|scheme>. This syntax was designed to be predictable, easy to
  hand-edit, and expose the complete internal structure of the document.

  <\tm-fragment>
    <verbatim|(with "mode" "math" (concat "x+y+" (frac "1" "2") "+" (sqrt
    "y+z")))>
  </tm-fragment>

  This syntax also represent documents as conventional s-expressions and is
  easy to process by <value|scheme> programs. You may also find it useful
  when debugging complex stylesheets.

  <tmdoc-copyright|2004|Joris van der Hoeven>

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