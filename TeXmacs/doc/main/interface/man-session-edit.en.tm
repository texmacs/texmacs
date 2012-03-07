<TeXmacs|1.0.7.14>

<style|<tuple|tmdoc|maxima>>

<\body>
  <tmdoc-title|Editing sessions>

  Inside input fields of sessions, the cursor keys have a special meaning:
  when moving upwards or downwards, you will move to previous or subsequent
  input fields. When moving to the left or to the right, you will never leave
  the input field; you should rather use the mouse for this.

  Some facilities for editing input, output and text fields are available in
  the <menu|Session|Field> menu. Keyboard shortcuts for inserting fields are
  <shortcut|(structured-insert-up)> (insert above) and
  <shortcut|(structured-insert-down)>. Keyboard shortcuts for removing
  matching text/input/output fields are <shortcut|(structured-remove-left)>
  (remove backwards) and <shortcut|(structured-remove-right)> (remove current
  fields).

  It is possible to create ``subsessions'' using <menu|Session|Session|Create
  subsession> or <shortcut|(structured-insert-right)>. In that case, the
  current input-output field becomes the body of an unfolded subsession. Such
  a subsession consists of an explanatory text together with the subsession
  body. Subsessions can be folded and unfolded using
  <shortcut|(dynamic-previous)> <abbr|resp.> <shortcut|(dynamic-next)>.
  Subsessions have a nice rendering on the screen when using the
  <tmpackage|varsession> package in <menu|Document|Use package|Program>.

  Notice that input/output fields and subsessions are foldable: when clicking
  on the prompt with the mouse, you may fold or unfold the entry to hide or
  show the output. For laptop presentations, this folding and unfolding
  process is done automatically when traversing your presentation. It is also
  possible to fold or unfold all fields in a session using
  <menu|Session|Session|Fold all fields> and <menu|Session|Session|Unfold all
  fields>.

  Other useful editing operations are <menu|Session|Session|Clear all
  fields>, which is useful for creating a demo session which will be executed
  later on, and <menu|Session|Split session>, which can be used for splitting
  a session into parts for inclusion into a paper.

  <\example>
    <label|session-example>A typical <name|Maxima> session is given below. If
    <name|Maxima> is present on your system, then you may put your cursor in
    one of the inputs, perform some edits, and try to reexecute it.

    <\session|maxima|default>
      <\output>
        Maxima 5.25.1 http://maxima.sourceforge.net

        using Lisp SBCL 1.0.51

        Distributed under the GNU Public License. See the file COPYING.

        Dedicated to the memory of William Schelter.

        The function bug_report() provides bug reporting information.
      </output>

      <\unfolded-io>
        <with|color|red|(<with|math-font-family|rm|%i>1) >
      <|unfolded-io>
        diff (x^x^x, x)
      <|unfolded-io>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o1>)
        >>x<rsup|x<rsup|x>>*<around*|(|x<rsup|x>*log
        <around*|(|x|)>*<around*|(|log <around*|(|x|)>+1|)>+x<rsup|x-1>|)>>>
      </unfolded-io>

      <\unfolded-io>
        <with|color|red|(<with|math-font-family|rm|%i>2) >
      <|unfolded-io>
        integrate (%o1, x)
      <|unfolded-io>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o2>)
        >>\<mathe\><rsup|\<mathe\><rsup|x*log <around*|(|x|)>>*log
        <around*|(|x|)>>>>
      </unfolded-io>

      <\unfolded-io>
        <with|color|red|(<with|math-font-family|rm|%i>3) >
      <|unfolded-io>
        integrate (x^5 / (x^2 - x + 17), x)
      <|unfolded-io>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o3>)
        >><frac|239*log <around*|(|x<rsup|2>-x+17|)>|2>+<frac|1361*arctan
        <around*|(|<frac|2*x-1|<sqrt|67>>|)>|<sqrt|67>>+<frac|3*x<rsup|4>+4*x<rsup|3>-96*x<rsup|2>-396*x|12>>>
      </unfolded-io>
    </session>
  </example>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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