<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Theorem-like environments>

  The <tmdtd|env-theorem> <abbr|d.t.d.> provides tags for the layout of
  theorem-like environments. The most important tags are

  <\description>
    <expand|item*|<markup|theorem*>>A macro for displaying a theorem-like
    environments. The first argument specifies the name of the theorem, like
    ``Theorem 1.2'' and the second argument contains the body of the theorem.
    This environment is used for environments defined by <markup|newtheorem>.

    <expand|item*|<markup|remark*>>Similar to <markup|theorem*>, but for
    remark-like environments.

    <expand|item*|<markup|exercise*>>Similar to <markup|theorem*>, but for
    exercise-like environments.

    <expand|item*|<markup|proof*>>Similar to <markup|theorem*>, but for
    proofs. This environment is mainly used for customizing the name of a
    proof, like in ``End of the proof of theorem 1.2''.\ 

    <expand|item*|<markup|dueto>>An environment which can be used to specify
    the inventors of a theorem.

    <expand|item*|<markup|corollary*>>For unnumbered corollaries. This
    environment is based on <markup|theorem*>.

    <expand|item*|<markup|proof>>For proofs of theorems. This environment is
    based on <markup|proof*>.
  </description>

  The following tags can be used for further customization of the
  environments.

  <\description>
    <expand|item*|<markup|theoremname>>A macro which controls the appearance
    of the names of theorem-like <em|and> remark-like environments. Most
    styles use bold face or small capitals.

    <expand|item*|<markup|exercisename>>Similar to <markup|theoremname>, but
    for exercises.

    <expand|item*|<markup|theoremsep>>The separator between the name of a
    theorem-like or remark-like environment and its main body. By default,
    this is a period followed by a space.

    <expand|item*|<markup|exercisesep>>Similar to <markup|theoremsep>, but
    for exercises.
  </description>

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
