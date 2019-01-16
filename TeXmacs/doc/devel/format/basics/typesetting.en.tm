<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|The typesetting process>

  In order to understand the <TeXmacs> document format well, it is useful to
  have a basic understanding about how documents are typeset by the editor.
  The typesetter mainly rewrites logical <TeXmacs> trees into physical
  <em|boxes>, which can be displayed on the screen or on paper (notice that
  boxes actually contain more information than is necessary for their
  rendering, such as information about how to position the cursor inside the
  box or how to make selections).

  The global typesetting process can be subdivided into two major parts
  (which are currently done at the same stage, but this may change in the
  future): evaluation of the <TeXmacs> tree using the stylesheet language,
  and the actual typesetting.

  The <hlink|typesetting primitives|../regular/regular.en.tm> are designed to
  be very fast and they are built-in into the editor. For instance, one has
  typesetting primitives for horizontal concatenations (<markup|concat>),
  page breaks (<markup|page-break>), mathematical fractions (<markup|frac>),
  hyperlinks (<markup|hlink>), and so on. The precise rendering of many of
  the typesetting primitives may be customized through the <hlink|built-in
  environment variables|../environment/environment.en.tm>. For instance, the
  environment variable <src-var|color> specifies the current color of
  objects, <src-var|par-left> the current left margin of paragraphs,
  <abbr|etc.>

  The <hlink|stylesheet language|../stylesheet/stylesheet.en.tm> allows the
  user to write new primitives (macros) on top of the built-in primitives. It
  contains primitives for defining macros, conditional statements,
  computations, delayed execution, <abbr|etc.> The stylesheet language also
  provides a special <markup|extern> tag which offers you the full power of
  the <scheme> extension language in order to write macros.

  It should be noticed that user-defined macros have two aspects. On the one
  hand they usually perform simple rewritings. For instance, the macro

  <\tm-fragment>
    <inactive*|<assign|seq|<macro|var|from|to|<active*|<math|<inactive*|<arg|var>><rsub|<inactive*|<arg|from>>>,\<ldots\>,<inactive*|<arg|var>><rsub|<inactive*|<arg|to>>>>>>>>
  </tm-fragment>

  is a shortcut in order to produce sequences like
  <math|a<rsub|1>,\<ldots\>,a<rsub|n>>. When macros perform simple rewritings
  like in this example, the children <src-arg|var>, <src-arg|from> and
  <src-arg|to> of the <markup|seq> tag remain <em|accessible> from within the
  editor. In other words, you can position the cursor inside them and modify
  them. User defined macros also have a synthetic or computational aspect.
  For instance, the dots of a <markup|seq> tag as above cannot be edited by
  the user. Similarly, the macro

  <\tm-fragment>
    <inactive*|<assign|square|<macro|x|<times|<arg|x>|<arg|x>>>>>
  </tm-fragment>

  serves an exclusively computational purpose. As a general rule, synthetic
  macros are sometimes easier to write, but the more accessibility is
  preserved, the more natural it becomes for the user to edit the markup.

  It should be noticed that <TeXmacs> also produces some auxiliary data as a
  byproduct of the typesetting product. For instance, the correct values of
  references and page numbers, as well as tables of contents, indexes,
  <abbr|etc.> are determined during the typesetting stage and memorized at a
  special place. Even though auxiliary data may be determined automatically
  from the document, it may be expensive to do so (one typically has to
  retypeset the document). When the auxiliary data are computed by an
  external plug-in, then it may even be impossible to perform the
  recomputations on certain systems. For these reasons, auxiliary data are
  carefully memorized and stored on disk when you save your work.

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>