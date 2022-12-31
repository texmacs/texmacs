<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Computational markup>

  In the menus <menu|Source|Arithmetic>, <menu|Source|Text>,
  <menu|Source|Tuple> and <menu|Source|Condition> you will find different
  primitives for computing with integers, strings, tuples and boolean values.
  For instance, in the following code, the <markup|new-important> tag defines
  a new \Pimportant tag\Q as well as a variant in red:

  <\tm-fragment>
    <inactive*|<assign|new-important|<macro|name|<quasi|<style-with|src-compact|none|<style-with|src-compact|none|<assign|<unquote|<arg|name>>|<macro|x|<with|font-series|bold|<arg|x>>>>><style-with|src-compact|none|<assign|<unquote|<merge|<arg|name>|-red>>|<macro|x|<with|font-series|bold|color|red|<arg|x>>>>>>>>>>
  </tm-fragment>

  Here we use the <markup|merge> primitive in order to concatenate two
  strings. The different computational primitives are described in more
  detail in the <hlink|corresponding section|../../format/stylesheet/prim-functional.en.tm>
  on the <hlink|<TeXmacs> primitives|../../format/stylesheet/stylesheet.en.tm>.

  <tmdoc-copyright|1998\U2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>