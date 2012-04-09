<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|The <name|Reduce> plug-in>

  <section|Installation>

  <name|Reduce> (<hlink|http://reduce-algebra.sourceforge.net|http://reduce-algebra.sourceforge.net>)
  is a free computer algebra system. There are two versions based on two lisp
  systems, <name|psl> and <name|csl>. Currently, the <TeXmacs> interface
  works only with <name|psl> based <name|Reduce>. Copy the file
  <verbatim|install/reducerc> to <verbatim|~/.reducerc> if you dont have this
  initialization file; otherwise, merge in with your <verbatim|~/.reducerc>.
  If you have <name|Reduce> <verbatim|html> documentation installed
  somewhere, set the environment variable <verbatim|REDUCE_HELP> to the full
  path to <verbatim|index.html>; then help will be available.

  <section|<name|Reduce> session>

  If you have <verbatim|redpsl> in your <verbatim|PATH>, your sessions menu
  will contain <verbatim|Reduce>. Clicking it will start a <name|Reduce>
  session.

  Each <name|Reduce> statement is terminated by <verbatim|;> or <verbatim|$>
  (it suppresses the output). If neither of them is contained in the current
  input, the plugin appends <verbatim|;> automatically. <menu|Reduce> menu
  allows one to load additional packages and to insert sceletons of some
  frequently used functions.

  You can switch <menu|Mathematical input> on. All usual notations (powers,
  fractions, square roots, etc.) work as expected. You can write matrices
  (and determinants) in 2-dimensional form. Also, integrals

  <\equation*>
    <big|int>f<around*|(|x|)> \<mathd\>x
  </equation*>

  work. Note that the differential <math|\<mathd\>> is obtained by <key|d>
  <key|tab> <key|Tab>; there should be no <key|*> before it, but you may
  insert <key|Space>. Definite integrals work, too, as well as sums like

  <\equation*>
    <big|sum><rsub|n=0><rsup|10>f<around*|(|n|)>
  </equation*>

  There are also indefinite sums

  <\equation*>
    <big|sum><rsub|n>f<around*|(|n|)>
  </equation*>

  similar forms of products are also supported.

  <section|Executable fold and scripting>

  You can use <menu|Insert> <menu|Fold> <menu|Executable> <menu|Reduce>. This
  produces a two-state fold in which you can write a <name|Reduce> statement.
  When you press <key|Enter>, <name|Reduce> is called to evaluate this
  statement, and it is replaced by the resulting formula. Later, going to
  this formula and pressing <key|Enter> again, you switch back to the source
  statement. You can edit it and execute again.

  You can also do <menu|Edit> <menu|Preferences> <menu|Scripts>
  <menu|Reduce>. Then you can write some formula, like

  <\equation*>
    y=<big|int>sin<around*|(|x|)> \<mathd\>x
  </equation*>

  Then you select a part of this formula, e.g., its right-hand side, and
  press <key|Ctrl>-<key|Enter>. The selected part is replaced by the result
  of its evaluation by <name|Reduce>, and the formula becomes

  <\equation*>
    y=-cos<around*|(|x|)>
  </equation*>

  <tmdoc-copyright|2012|Andrey Grozin>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
  </collection>
</initial>