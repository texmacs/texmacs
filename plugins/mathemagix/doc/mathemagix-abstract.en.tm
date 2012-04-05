<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|The <name|Mathemagix> system>

  <mathemagix> is a free computer algebra system under development available
  for download from <hlink|<samp|http://www.mathemagix.org>|http://www.mathemagix.org/>.
  It consists of the following ingredients.<strong|>

  <strong|><subsection*|The <mathemagix> language>

  <mathemagix> provides a new high level language, which is imperative,
  strongly typed, with polymorphim and parametrized types. <mathemagix> can
  be used as an ``extension language'', <abbr|i.e.> \ easy to embed into
  other applications and to extend with existing libraries written in other
  languages like C or C++. An interesting feature is that this extension
  mechanism supports template types.

  Currently, only a rather slow interpreter <name|Mmx-light> is available,
  but a compiler is under development. All necessary type verifications are
  done during the compilation phase.

  <subsection*|The <mathemagix> packages>

  Standard libraries are available for algebraic computation (large numbers,
  polynomials, power series, matrices, <abbr|etc.> based on FFT and other
  fast algorithms) for exact and approximate computation. This should make
  <mathemagix> particularly suitable as a bridge between symbolic computation
  and numerical analysis.\ 

  These packages written in C++ are connected to the interpreter (and later
  to the compiler under development), but can also be used independently as
  standalone libraries. Separate documentation for each of the packages is
  also available.

  <tmdoc-copyright|2012|Gregoire Lecerf>

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