<TeXmacs|1.0.7.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Functional operators>

  Functional operators are used for computational purposes during the
  typesetting phase, such as increasing counters, localizing strings like
  ``theorem'' and so on. A fundamental set of basic functional operators are
  built-in primitives. New functional operators can easily be added using the
  <markup|extern> primitive. Functional operators operate on five main types
  of arguments: strings, numbers, lengths, booleans and tuples. Some
  operators are overloaded, so that they can be used for several types.

  <\traverse>
    <branch|Operations on text|prim-strings.en.tm>

    <branch|Arithmetic operations|prim-arith.en.tm>

    <branch|Boolean operations|prim-boolean.en.tm>

    <branch|Operations on tuples|prim-tuples.en.tm>
  </traverse>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>