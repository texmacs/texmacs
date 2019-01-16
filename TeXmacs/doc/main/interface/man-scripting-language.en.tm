<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Plug-ins as scripting languages>

  <TeXmacs> provides a few other kinds of additional interfaces to external
  systems in addition to shell-like interfaces. First of all, it is possible
  to insert a so called \Pexecutable switch\Q anywhere in the document using
  <menu|Insert|Fold|Executable>.

  For instance, if <name|Maxima> is installed on your system, then
  <menu|Insert|Fold|Executable|Maxima> should yield something like
  <script-input|maxima|default||>. You may enter a<nbsp><name|Maxima>
  expression in the yellow part of this markup, say
  <script-input|maxima|default|diff(x^x,x)|>. Using <shortcut|(kbd-return)>,
  you may now switch back and forth between the unevaluated input and the
  evaluated output <script-output|maxima|default|diff(x^x,x)|<math|x<rsup|x>*<around*|(|log
  <around*|(|x|)>+1|)>>>. Using <shortcut|(kbd-shift-return)>, you enable
  multi-line input. This kind of executable switches are very useful for
  plug-ins such as <name|Dra<TeX>>, <name|Eukleides>, <name|Feynmf>,
  <abbr|etc.>, which are mainly used for the efficient computation and
  insertion of special graphics inside <TeXmacs> documents.

  Some plug-ins such as <name|Maxima> can even be selected as a <em|scripting
  language> using <menu|Document|Scripts|Maxima>. When doing so, a special
  <menu|Maxima> menu will appear, which allows for many useful operations
  directly on formulas. For instance, when putting the cursor inside the
  formula <math|1+1> and pressing <shortcut|(script-eval)> or
  <menu|Evaluate>, the formula gets evaluated automatically to yield
  <math|2>.

  If a plug-in can be used as a scripting language, then it is possible to
  create executable switches with links between them. More precisely,
  assuming that you selected a scripting language from
  <menu|Document|Scripts>, you may insert a new <em|executable input field>
  using <shortcut|(make-calc-input)> or <menu|Insert|Link|Executable input
  field>. As before, when pressing <key|return>, the current input is
  evaluated and you will see the corresponding output; you may switch back to
  the input by pressing <key|return> once more.

  Contrary to executable switches, you may attach an identifier to the
  executable input field by deactivating the field or by editing the
  <samp|Ref> field in the focus bar. Inside other executable input fields,
  you may then refer to the value of the field by inserting a <em|field
  reference> using <shortcut|(make 'calc-ref)> or <menu|Insert|Link|Field
  reference>. As a variant to executable input fields, you may sometimes
  prefer to insert plain <em|input fields> using <shortcut|(make-calc-inert)>
  or <menu|Insert|Link|Input field>. These fields can only be used as inputs
  and pressing <key|return> inside such a field will only recompute those
  other fields which depend on it.

  <\example>
    The executable input fields may for instance be nice in pedagogic
    documents in which parts of the document may be modified and recomputed
    by the reader. For instance, evaluation of the input fragment

    <\quote-env>
      The derivative of <with|prog-scripts|maxima|<calc-inert|function|<math|x<rsup|x>>>>
      equals <with|prog-scripts|maxima|<calc-input|derivative|diff(<calc-ref|function>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)>>>>.

      The second derivative is given by <with|prog-scripts|maxima|<calc-input|second|diff(<calc-ref|derivative>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)><rsup|2>+x<rsup|x-1>>>>.
    </quote-env>

    yields

    <\quote-env>
      The derivative of <with|prog-scripts|maxima|<calc-inert|function2|<math|x<rsup|x>>>>
      equals <with|prog-scripts|maxima|<calc-output|derivative2|diff(<calc-ref|function2>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)>>>>.

      The second derivative is given by <with|prog-scripts|maxima|<calc-output|second2|diff(<calc-ref|derivative2>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)><rsup|2>+x<rsup|x-1>>>>.
    </quote-env>

    Of course, if the reader changes the input function <math|x<rsup|x>> into
    something else and presses <key|return>, then the first and second
    derivatives will be updated automatically.
  </example>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>