<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|pari|old-spacing>>

<\body>
  <tmdoc-title|Structured variants>

  When creating an environment like a theorem, an equation or a list, it
  frequently happens that one would like to change the environment <em|a
  posteriori>. The keyboard shortcuts<nbsp><shortcut|(variant-circulate
  (focus-tree) #t)> and <shortcut|(variant-circulate (focus-tree) #f)> allow
  you to cycle through the list of <em|structured variants> of the innermost
  tag, in forward or backward direction, respectively.

  For instance, assuming that you are inside a theorem, pressing
  <shortcut|(variant-circulate (focus-tree) #t)> several times will change
  the theorem into a proposition, a lemma, a corollary, a conjecture, and
  finally back into a theorem. The <shortcut|(variant-circulate (focus-tree)
  #f)> key allows you to cycle in the reverse direction:
  theorem<nbsp><math|<op|\<rightarrow\>>>
  conjecture<nbsp><math|<op|\<rightarrow\>>>
  corollary<nbsp><math|<op|\<rightarrow\>>>
  lemma<nbsp><math|<op|\<rightarrow\>>> proposition<nbsp><math|<op|\<rightarrow\>>>
  theorem.

  In the case of mathematical formulas, the <shortcut|(variant-circulate
  (focus-tree) #t)> shortcut allows you to change an inline formula such as
  <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> into the displayed formula

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>
  </equation*>

  while taking care of potential \Ptrailing spaces and punctuation signs\Q.

  <TeXmacs> also provides the <shortcut|(numbered-toggle (focus-tree))>
  shortcut for turning numbered environments into unnumbered environments and
  <em|vice versa>. This works for most common environments like theorems,
  remarks, tables, equations, etc. Notice that <shortcut|(numbered-toggle
  (focus-tree))> also turns an unnumbered itemize environment into an
  enumeration and <em|vice versa>, whereas <shortcut|(variant-circulate
  (focus-tree) #t)> allows you to cycle between the available kinds of list
  items (bullets, dashes, arrows, <abbr|etc.>).

  Folding and unfolding provides yet another example of an interesting way to
  toggle between several environments. Inside a computer algebra session such
  as

  <\session|pari|default>
    <\unfolded-io|Pari] >
      factor (x^15 - 1)
    <|unfolded-io>
      <with|color|magenta|%1 = <math|<with|color|blue|<matrix|<tformat|<table|<row|<cell|x-1>|<cell|1>>|<row|<cell|x<rsup|2>+x+1>|<cell|1>>|<row|<cell|x<rsup|4>+x<rsup|3>+x<rsup|2>+x+1>|<cell|1>>|<row|<cell|x<rsup|8>-x<rsup|7>+x<rsup|5>-x<rsup|4>+x<rsup|3>-x+1>|<cell|1>>>>>>>>
    </unfolded-io>
  </session>

  you may click on \P<verbatim|<with|color|dark brown|Pari]>>\Q in order to
  fold the output (only the input remains visible) and click once again in
  order to unfold back to the original state. The <shortcut|(alternate-toggle
  (focus-tree))> shortcut achieves the same effect. There various other
  foldable environments, most of which are available through
  <menu|Insert|Fold>.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>