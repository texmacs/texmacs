<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Incorporating mathematical formulas into documents>

  <TeXmacs> provides three main ways in order to enter a mathematical
  formulas into the main text:

  <\description>
    <item*|<menu|Insert|Mathematics|Inline formula> or <key|$>>

    This entry corresponds to small <em|inline formulas> like
    <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> inside a textual paragraph. Note
    that formulas are typeset specially so they do not take too much vertical
    space. For example, limits are always displayed on the right. Limits can
    be displayed below in formulas with <menu|Format|Display style|on>. In
    formulas, display style is off by default.

    <item*|<menu|Insert|Mathematics|Displayed formula> or
    <shortcut|(make-equation*)>>

    This entry is used for entering bigger <em|displayed equations>, like

    <\equation*>
      x<rsup|n>+y<rsup|n>=z<rsup|n>,
    </equation*>

    which are typeset in a paragraph of their own. You may use the shortcut
    <shortcut|(numbered-toggle (focus-tree))> in order to give the equation a
    number (or to remove the number of an equation). Also,
    <shortcut|(variant-circulate (focus-tree) #t)> allows you to switch
    between inline formulas and displayed equations.

    <item*|<menu|Insert|Mathematics|Several equations> or
    <shortcut|(make-eqnarray*)>>

    This entry allows you to create an <markup|eqnarray*>, a three columns
    wide table-like environment (see <hlink|creating
    tables|../table/man-create-table.en.tm>). This environment is typically
    used for lists of multiple relations like

    <\eqnarray*>
      <tformat|<table|<row|<cell|x+0>|<cell|=>|<cell|x>>|<row|<cell|x+<around|(|\<um\>x|)>>|<cell|=>|<cell|0>>|<row|<cell|x+y>|<cell|=>|<cell|y+x>>|<row|<cell|<around|(|x+y|)>+z>|<cell|=>|<cell|x+<around|(|y+z|)>>>>>
    </eqnarray*>

    The first column is centered to the right, the second one at the middle
    and the last one at the left. Another typical use of the
    <markup|eqnarray*> environment is a step by step computation

    <\eqnarray*>
      <tformat|<table|<row|<cell|<around*|(|\<mathe\><rsup|sin x>+sin
      \<mathe\><rsup|x>|)><rprime|'>>|<cell|=>|<cell|<around*|(|\<mathe\><rsup|sin
      x>|)><rprime|'>+<around*|(|sin \<mathe\><rsup|x>|)><rprime|'>>>|<row|<cell|>|<cell|=>|<cell|<around|(|sin
      x|)><rprime|'>*\<mathe\><rsup|sin x>+<around*|(|\<mathe\><rsup|x>|)><rprime|'>*cos
      \<mathe\><rsup|x>>>|<row|<cell|>|<cell|=>|<cell|\<mathe\><rsup|sin
      x>*cos x+\<mathe\><rsup|x>*cos \<mathe\><rsup|x>,>>>>
    </eqnarray*>

    in which many entries of the left column are left open.
  </description>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>