<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Standard lists>

  The standard <TeXmacs> lists are defined in <tmdtd|std-list>. The
  unnumbered lists are:

  <\description>
    <expand|item*|<markup|itemize>>The tag before each item depends on the
    nesting depth.

    <expand|item*|<markup|itemize-minus>>Uses <with|mode|math|-> for the tag.

    <expand|item*|<markup|itemize-dot>>Uses <with|mode|math|\<bullet\>> for
    the tag.

    <expand|item*|<markup|itemize-arrow>>Uses <with|mode|math|\<rightarrow\>>
    for the tag.
  </description>

  Numbered lists correspond to the following environments:

  <\description>
    <expand|item*|<markup|enumerate>>The kind of number before each item
    depends on the nesting depth.

    <expand|item*|<markup|enumerate-numeric>>Number the items by 1, 2, 3,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-roman>>Number the items by i, ii, iii,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-Roman>>Number the items by I, II, III,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-alpha>>Number the items by a), b), c),
    <abbr|etc.>

    <expand|item*|<markup|enumerate-Alpha>>Number the items by A, B, C,
    <abbr|etc.>
  </description>

  The following environments can be used for descriptive lists.

  <\description>
    <expand|item*|<markup|description>>The environment for default
    descriptive lists (usually <markup|description-compact>).

    <expand|item*|<markup|description-compact>>Align the left hand sides of
    the items in the list and put their descriptions shortly behind it.

    <expand|item*|<markup|description-dash>>Similar to
    <markup|description-compact>, but use a  to seperate each item from its
    description.

    <expand|item*|<markup|description-align>>Align the left hand sides of the
    descriptions, while aligning the items to the right.

    <expand|item*|<markup|description-long>>Put the items and their
    descriptions on distinct lines.
  </description>

  New items in a list are indicated through the <markup|item> tag or the
  unary <markup|item*> tag in the case of descriptions. Developers will also
  find a few additional, but unstable, macros in <tmdtd|std-list> for
  defining additional list structures.

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
