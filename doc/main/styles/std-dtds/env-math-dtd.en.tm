<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Mathematical environments>

  The <tmdtd|env-math> <abbr|d.t.d.> specifies which mathematical
  environments can be used inside text-mode. In other words, the environments
  should be used inside text-mode, but their bodies contain mathematical
  formulas or tables of mathematical formulas.

  <\description>
    <expand|item*|<markup|equation>>A numbered equation.

    <expand|item*|<markup|equation*>>An unnumbered equation.

    <expand|item*|<markup|eqnarray>>An array of numbered equations (should
    not be used yet).

    <expand|item*|<markup|eqnarray*>>An array of unnumbered equations.
  </description>

  Inside the <markup|eqnarray*> environment, you can use the
  <markup|eqnumber> tag in order to number the equation.\ 

  <\warning>
    The numbering of equations inside tables is not yet as it should be. In
    particular, the <markup|eqnarray> tag is equivalent to <markup|eqnarray*>
    at the moment. Later on, when the <markup|eqnarray> tag will be
    implemented correctly, you will also have a <markup|nonumber> tag in
    order to suppress the number of an equation, and a style package for
    numbering equations at the left hand side.
  </warning>

  <\warning>
    There is no option for numbering equations at the left hand side
    available yet. Nevertheless, you may use the manual tag
    <markup|leqnumber> for this. You also have a tag <markup|nextnumber>
    which directly display the next number and increases the equation
    counter.
  </warning>

  <\warning>
    We do not encourage the use of the AMS-<TeX> environments
    <verbatim|align>, <verbatim|gather> and <verbatim|split>. Nevertheless,
    they are available under the names <markup|align>, <markup|gather>,
    <markup|eqsplit> together with their variants <markup|align*>,
    <markup|gather*> and <markup|eqsplit*>. In the future, we plan to provide
    more powerful environments.
  </warning>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
