<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Computational markup>

  In the menus <menu|Source|Arithmetic>, <menu|Source|Text>,
  <menu|Source|Tuple> and <menu|Source|Condition> you will find different
  primitives for computing with integers, strings, tuples and boolean values.
  For instance, in the following code, the <markup|new-important> tag defines
  a new ``important tag'' as well as a variant in red:

  <\tm-fragment>
    <inactive*|<assign|new-important|<macro|name|<quasi|<style-with|src-compact|none|<style-with|src-compact|none|<assign|<unquote|<arg|name>>|<macro|x|<with|font-series|bold|<arg|x>>>>><style-with|src-compact|none|<assign|<unquote|<merge|<arg|name>|-red>>|<macro|x|<with|font-series|bold|color|red|<arg|x>>>>>>>>>>
  </tm-fragment>

  Here we use the <markup|merge> primitive in order to concatenate two
  strings. The different computational primitives are described in more
  detail in the <hyper-link|corresponding
  section|../../format/primitives/prim-functional.en.tm> on the
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