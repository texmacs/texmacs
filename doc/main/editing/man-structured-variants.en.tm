<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Structured variants>

  When creating an environment like a theorem, an equation or a list, it
  frequently happens that one would like to change the environment <em|a
  posteriori>. This can be done using the keyboard shortcuts
  <key|C-<key-tab>> and <key|C-S-<key-tab>> for cycling through the list of
  <em|structured variants> of the innermost tag in a direct or inverse
  manner.

  For instance, assuming that you are inside a theorem, pressing
  <key|C-<key-tab>> several times will change the theorem into a proposition,
  a lemma, a corollary, a conjecture and back into a theorem. The
  <key|C-S-<key-tab>> key allows you to cycle in the inverse direction:
  theorem<nbsp><with|mode|math|<op|\<rightarrow\>>>
  conjectur<nbsp><with|mode|math|<op|\<rightarrow\>>>
  corollary<nbsp><with|mode|math|<op|\<rightarrow\>>>
  lemma<nbsp><with|mode|math|<op|\<rightarrow\>>>
  proposition<nbsp><with|mode|math|<op|\<rightarrow\>>> theorem.

  In the case of mathematical formulas, the <key|C-<key-tab>> shortcuts
  allows you to change an inline formula like
  <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>> into a displayed formula
  like

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>
  </equation*>

  while taking care of potential ``trailing spaces and punctuation signs''.

  <TeXmacs> also provides the <key|A-*> shortcut for turning numbered
  environments into unnumbered environments and <em|vice versa>. This works
  for most common environments like theorems, remarks, tables, equations,
  etc. Notice that <key|A-*> also turns an unnumbered itemize environment
  into an enumeration and <em|vice versa>, whereas <key|C-<key-tab>> allows
  you to cycle between the available kinds of list items (bullets, dashes,
  arrows, <abbr|etc.>).

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

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