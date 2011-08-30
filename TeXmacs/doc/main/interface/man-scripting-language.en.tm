<TeXmacs|1.0.7.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Plugins as scripting languages>

  <TeXmacs> provides a few other kinds of additional interfaces to external
  systems in addition to shell-like interfaces. First of all, it is possible
  to insert a so called ``executable switch'' anywhere in the document using
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
  plugins such as <name|Dra<TeX>>, <name|Eukleides>, <name|Feynmf>,
  <abbr|etc.>, which are mainly used for the efficient computation and
  insertion of special graphics inside <TeXmacs> documents.

  Some plugins such as <name|Maxima> can even be selected as a <em|scripting
  language> using <menu|Document|Scripts|Maxima>. When doing so, a special
  <menu|Maxima> menu will appear, which allows for many useful operations
  directly on formulas. For instance, when putting the cursor inside the
  formula <math|1+1> and pressing <shortcut|(kbd-return)> or <menu|Evaluate>,
  the formula gets evaluated automatically to yield <math|2>.

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