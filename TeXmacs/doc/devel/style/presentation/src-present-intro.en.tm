<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|ASCII-based or tree-based editing: an intricate choice>

  Most users are used to edit source code using a conventional editor like
  <name|Emacs>, while presenting the source code in ASCII format. Since all
  <TeXmacs> documents are stored as <hlink|trees|../../../devel/format/basics/basics.en.tm>,
  an interesting but complicated question is which format is most suitable
  for editing such documents. One option is to represent the tree using an
  ASCII-based format, such as XML, Scheme, or the native format for storing
  files on a disk. The other option is to edit the trees as such, making no
  fundamental distinction between source code and normal documents.

  In <TeXmacs> we have chosen to implement the second option. More precisely,
  any document can be edited in \Psource mode\Q, which is merely a mode for
  rendering the document in a way which makes its tree structure particularly
  apparent. It may be instructive to take an arbitrary document of yours and
  to take a look at it in \Psource mode\Q by enabling
  <menu|Document|Source|Edit source tree>.

  The choice between ASCII-based editing and tree-based editing is
  non-trivial, because <TeXmacs> style files and packages have a double
  nature: they may be seen as programs which specify how to render macros,
  but these programs naturally contain ordinary content. There are several
  reasons why users often prefer to edit source code in an ASCII-based
  format:

  <\enumerate>
    <item>It is easy to manually format the code so as to make it more
    readable.

    <item>In particular, it is easy to add comments.

    <item>Standard editors like <name|Emacs> provide tools for automatic
    highlighting, indentation, <abbr|etc.>

    <item><label|structure-constraints>One is not constraint by any
    \Pstructure\Q during the editing phase.
  </enumerate>

  Our approach is to reproduce as much of the above advantages in a
  structured document environment. Although point
  <reference|structure-constraints> will obviously be hard to meet when
  following this approach, we believe that the first three advantages might
  actually become greater in a structured environment. However, this requires
  a more profound understanding of how users format and edit source code.

  For instance, consider a piece of manually formatted code like

  <\cpp-code>
    if (cond) hop \ \ = 2;

    else \ \ \ \ \ holala= 3;
  </cpp-code>

  Clearly, the user had a particular formatting policy when writing this
  code. However, this policy does not appear in the document: manual
  intervention will be necessary if the variable <verbatim|cond> is renamed
  <verbatim|c>, or if the variable <verbatim|holala> is renamed
  <verbatim|hola>.

  At the moment, <TeXmacs> provides no tools for dealing with the above
  example in an automatic way, but a few tools are already provided. For
  instance, the user is given a great amount of control on how to indent
  source code and reasonable defaults are provided as a function of the
  structure. We also provide high level environments for comments and
  structured highlighting. Further tools will be developed later and we are
  open for any suggestions from our users.

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