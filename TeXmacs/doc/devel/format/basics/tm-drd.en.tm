<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Data relation descriptions>

  <paragraph*|The rationale behind <abbr|D.R.D.>s>

  One major advantage of <TeXmacs> is that the editor uses general trees as
  its data format. Like for <no-break>XML, this choice has the advantages of
  being simple to understand and making documents easy to manipulate by
  generic tools. However, when using the editor for a particular purpose, the
  data format usually needs to be restricted to a subset of the set of all
  possible trees.

  In XML, one uses Data Type Definitions (<abbr|D.T.D.>s) in order to
  formally specify a subset of the generic XML format. Such a <abbr|D.T.D.>
  specifies when a given document is valid for a particular purpose. For
  instance, one has <abbr|D.T.D.>s for documents on the web (<name|XHTML>),
  for mathematics <no-break>(<name|MathML>), for two-dimensional graphics
  (<name|SVG>) and so on. Moreover, up to a certain extent, XML provides
  mechanisms for combining such <abbr|D.T.D.>s. Finally, a precise
  description of a <abbr|D.T.D.> usually also provides some kind of reference
  manual for documents of a certain type.

  In <TeXmacs>, we have started to go one step further than <abbr|D.T.D.>s:
  besides being able to decide whether a given document is valid or not, it
  is also very useful to formally describe certain properties of the
  document. For instance, in an interactive editor, the numerator of a
  fraction may typically be edited by the user (we say that it is
  <em|accessible>), whereas the URL of a hyperlink is only editable on
  request. Similarly, certain primitives like <markup|itemize> correspond to
  block content, whereas other primitives like <markup|sqrt> correspond to
  inline content. Finally, certain groups of primitives, like
  <markup|chapter>, <markup|section>, <markup|subsection>, <abbr|etc.> behave
  similarly under certain operations, like conversions.

  A Data Relation Description (<abbr|D.R.D.>) consists of a Data Type
  Definition, together with additional logical properties of tags or document
  fragments. These logical properties are stated using so called <em|Horn
  clauses>, which are also used in logical programming languages such as
  Prolog. Contrary to logical programming languages, it should nevertheless
  be relatively straightforward to determine the properties of tags or
  document fragments, so that certain database techniques can be used for
  efficient implementations. At the moment, we only started to implement this
  technology (and we are still using lots of C++ hacks instead of what has
  been said above), so a more complete formal description of <abbr|D.R.D.>s
  will only be given at a later stage.

  One major advantage of the use of <abbr|D.R.D.>s is that it is not
  necessary to establish rigid hierarchies of object classes like in object
  oriented programming. This is particularly useful in our context, since
  properties like accessibility, inline-ness, <abbr|etc.> are quite
  independent one from another. In fact, where <abbr|D.T.D.>s may be good
  enough for the description of passive documents, more fine-grained
  properties are often useful when manipulating documents in a more
  interactive way.

  <paragraph*|Current <abbr|D.R.D.> properties and applications>

  Currently, the <abbr|D.R.D.> of a document contains the following
  information:

  <\itemize>
    <item>The possible arities of a tag.

    <item>The accessibility of a tag and its children.
  </itemize>

  In the near future, the following properties will be added:

  <\itemize>
    <item>Inline-ness of a tag and its children.

    <item>Tabular-ness of a tag and its children.

    <item>Purpose of a tag and its children.
  </itemize>

  The above information is used (among others) for the following
  applications:

  <\itemize>
    <item>Natural default behaviour when creating/deleting tags or children
    (automatic insertion of missing arguments and removal of tags with too
    little children).

    <item>Only traverse accessible nodes during searches, spell-checking,
    <abbr|etc.>

    <item>Automatic insertion of <markup|document> or <markup|table> tags
    when creating block or tabular environments.

    <item>Syntactic highlighting in source mode as a function of the purpose
    of tags and arguments.
  </itemize>

  <paragraph*|Determination of the <abbr|D.R.D.> of a document>

  <TeXmacs> associate a unique <abbr|D.R.D.> to each document. This
  <abbr|D.R.D.> is determined in two stages. First of all, <TeXmacs> tries to
  heuristically determine <abbr|D.R.D.> properties of user-defined tags, or
  tags which are defined in style files. For instance, when the user defines
  a tag like

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|name|Hello <arg|name>!>>>
  </tm-fragment>

  <TeXmacs> automatically notices that <markup|hi> is a macro with one
  element, so it considers <math|1> to be the only possible arity of the
  <markup|hi> tag. Notice that the heuristic determination of the
  <abbr|D.R.D.> is done interactively: when defining a macro inside your
  document, its properties will automatically be put into the <abbr|D.R.D.>
  (assuming that you give <TeXmacs> a small amount of free time of the order
  of a second; this minor delay is used to avoid compromising the reactivity
  of the editor).

  Sometimes the heuristically defined properties are inadequate. For this
  case, <TeXmacs> provides the <markup|drd-props> tag in order to
  <hlink|manually override|../stylesheet/prim-macro.en.tm> the default
  properties.

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