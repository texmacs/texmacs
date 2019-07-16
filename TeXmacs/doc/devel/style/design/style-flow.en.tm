<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Control flow>

  Besides sequences of instructions, which can be achieved using the
  <markup|concat> primitive, and the mechanism of macro expansion, the
  <TeXmacs> style-sheet language provides a few other primitive for affecting
  the control flow: <markup|if>, <markup|case>, <markup|while> and
  <markup|for-each>. These primitives are available from the
  <menu|Source|Control flow> menu. However, we have to warn the user that the
  conditional constructs are quite fragile: they only apply to inline content
  and the accessibility of macro arguments should not to much depend on the
  conditions.

  The most important primitive <markup|if>, which can be entered using
  <shortcut|(make 'if)>, allows for basic conditional typesetting:

  <\tm-fragment>
    <inactive*|<assign|appendix|<\macro|title|body>
      <style-with|src-compact|none|<compound|<if|<long-document>|<value|chapter-appendix>|<value|section-appendix>>|<arg|title>|<arg|body>>>
    </macro>>>
  </tm-fragment>

  In this example, <markup|appendix> is a block environment consisting of a
  title and a body, and which is rendered as a chapter for long documents and
  as a section for short ones. Notice that the following implementation would
  have been incorrect, since the <markup|if> primitive currently only works
  for inline content:

  <\tm-fragment>
    <inactive*|<assign|appendix|<\macro|title|body>
      <style-with|src-compact|none|<if|<long-document>|<chapter-appendix|<arg|title>|<arg|body>>|<section-appendix|<arg|title>|<arg|body>>>>
    </macro>>>
  </tm-fragment>

  The <markup|if> primitive may also be used in order to implement optional
  arguments:

  <\tm-fragment>
    <inactive*|<assign|hey|<macro|first|second|<style-with|src-compact|none|<if|<equal|<arg|second>|<uninit>>|Hey
    <arg|first>, you look lonely today...|Hey <arg|first> and <arg|second>,
    you form a nice couple!>>>>>
  </tm-fragment>

  However, <TeXmacs> is not clever enough to detect which arguments are
  optional and which arguments are accessible (<abbr|i.e.> which arguments
  can be edited by the user). Therefore, you will have to manually give this
  information using the <markup|drd-props> primitive. The <markup|case>,
  <markup|while> and <markup|for-each> primitives are explained in more
  detail in the <hyper-link|corresponding
  section|../../format/primitives/prim-control.en.tm> on the
  <hyper-link|<TeXmacs> primitives|../../format/primitives/primitives.en.tm>.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>