<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Assignments>

  All user defined <TeXmacs> macros and style variables are stored in the
  \Pcurrent typesetting environment\Q. This environment associates a tree
  value to each string variable. Variables whose values are macros correspond
  to new primitives. The others are ordinary environment variables. The
  primitives for operating on the environment are available from
  <menu|Source|Define>.

  You may permanently change the value of an environment variable using the
  <markup|assign> primitive, as in the example

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hi there!>>>
  </tm-fragment>

  You may also locally change the values of one or several environment
  variables using the <markup|with> primitive:

  <\tm-fragment>
    <inactive*|<with|font-series|bold|color|red|Bold red text>>
  </tm-fragment>

  The value of an environment variable may be retrieved using the
  <markup|value> primitive. This may for instance be used in order to
  increase a counter:

  <\tm-fragment>
    <inactive*|<assign|my-counter|<plus|<value|my-counter>|1>>>
  </tm-fragment>

  Finally, you may associate logical properties to environment variables
  using the <markup|drd-props> primitive. This is explained in more detail in
  the section about <hlink|macro primitives|../../format/stylesheet/prim-macro.en.tm>.

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