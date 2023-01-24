<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Macro expansion>

  The main interest of the <TeXmacs>' style-sheet language is the possibility
  to define macros. These come in three flavours: ordinary macros, macros
  which take an arbitrary number of arguments and external macros, whose
  expansion is computed by <name|Scheme> or a plug-in. The macro-related
  primitives are available from the <menu|Source|Macro> menu. Below, we will
  only describe the ordinary macros. For more details, we refer to the
  section about <hlink|macro primitives|../../format/stylesheet/prim-macro.en.tm>.

  Ordinary macros are usually defined using

  <\tm-fragment>
    <inactive*|<assign|my-macro|<macro|<active*|x<rsub|1>>|<active*|<math|\<cdots\>>>|<active*|x<rsub|n>>|body>>>
  </tm-fragment>

  After such an assignment, <markup|my-macro> becomes a new primitive with
  <math|n> arguments, which may be called using

  <\tm-fragment>
    <inactive|<my-macro|<active*|y<rsub|1>>|<active*|<math|\<cdots\>>>|<active*|y<rsub|n>>>>
  </tm-fragment>

  Inside the body of the macro, the <markup|arg> primitive may be used to
  retrieve the values of the arguments to the macro.

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <arg|name>, you look nice
    today!>>>
  </tm-fragment>

  It is possible to call a macro with less or more arguments than the
  expected number. Superfluous arguments are simply ignored. Missing
  arguments take the nullary <markup|uninit> primitive as value:

  <\tm-fragment>
    <inactive*|<assign|hey|<macro|first|second|<style-with|src-compact|none|<if|<equal|<arg|second>|<uninit>>|Hey
    <arg|first>, you look lonely today...|Hey <arg|first> and <arg|second>,
    you form a nice couple!>>>>>
  </tm-fragment>

  We finally notice that you are allowed to compute with macros, in a similar
  way as in functional programming, except that our macros are not closures
  (yet). For instance:

  <\tm-fragment>
    <inactive|<assign|my-macro-copy|<inactive|<value|my-macro>>>>
  </tm-fragment>

  The <markup|compound> tag may be used to apply macros which are the result
  of a computation:

  <\tm-fragment>
    <inactive*|<assign|overloaded-hi|<macro|name|<style-with|src-compact|none|<compound|<if|<nice-weather>|<value|happy-hi>|<value|sad-hi>>|<arg|name>>>>>>
  </tm-fragment>

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