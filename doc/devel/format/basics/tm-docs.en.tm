<TeXmacs|1.0.3.11>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs> documents>

  Whereas <TeXmacs> document fragments can be general <TeXmacs> trees,
  <TeXmacs> documents are trees of a special form which we will describe now.
  The root of a <TeXmacs> document is necessarily a <markup|document> tag.
  The children of this tag are necessarily of one of the following forms:

  <\explain|<explain-macro|TeXmacs|version><explain-synopsis|<TeXmacs>
  version>>
    This mandatory tag specifies the version of <TeXmacs> which was used to
    save the document.
  </explain>

  <\explain|<explain-macro|project|ref><explain-synopsis|part of a project>>
    An optional project to which the document belongs.
  </explain>

  <\explain>
    <explain-macro|style|version>

    <explain-macro|style|<with|font-shape|right|<explain-macro|tuple|style|pack-1|<with|mode|math|\<cdots\>>|pack-n>>><explain-synopsis|style
    and packages>
  <|explain>
    An optional style and additional packages for the document.
  </explain>

  <\explain|<explain-macro|body|content><explain-synopsis|body of the
  document>>
    This mandatory tag specifies the body of your document.
  </explain>

  <\explain|<label|initial-env><explain-macro|initial|table><explain-synopsis|initial
  environment>>
    Optional specification of the initial environment for the document, with
    information about the page size, margins, <abbr|etc.>. The
    <src-arg|table> is of the form <explain-macro|collection|binding-1|<with|mode|math|\<cdots\>>|binding-n>.
    Each <src-arg|binding-<no-break>i> is of the form
    <explain-macro|associate|var-i|val-i> and associates the initial value
    <src-arg|val-i> to the environment variable <src-arg|var-i>. The initial
    values of environment variables which do not occur in the table are
    determined by the style file and packages.
  </explain>

  <\explain|<explain-macro|references|table><explain-synopsis|references>>
    An optional list of all valid references to labels in the document. Even
    though this information can be automatically recovered by the typesetter,
    this recovery requires several passes. In order to make the behaviour of
    the editor more natural when loading files, references are therefore
    stored along with the document.

    The <src-arg|table> is of a similar form as above. In this case a tuple
    is associated to each label. This tuple is either of the form
    <explain-macro|tuple|content|page-nr> or
    <explain-macro|tuple|content|page-nr|file>. The <src-arg|content>
    corresponds to the displayed text when referring to the label,
    <src-arg|page-nr> to the corresponding page number, and the optional
    <src-arg|file> to the file where the label was defined (this is only used
    when the file is part of a project).
  </explain>

  <\explain|<explain-macro|auxiliary|table><explain-synopsis|auxiliary data
  attached to the file>>
    This optional tag specifies all auxiliary data attached to the document.
    Usually, such auxiliary data can be recomputed automatically from the
    document, but such recomputations may be expensive and even require tools
    which are not necessarily installed on your system. The <src-arg|table>,
    which is specified in a similar way as above, associates auxiliary
    content to a key. Standard keys include <verbatim|bib>, <verbatim|toc>,
    <verbatim|idx>, <verbatim|gly>, <abbr|etc.>
  </explain>

  <\example>
    An article with the simple text ``hello world!'' is represented as

    <\equation*>
      <tree|<with|mode|text|<markup|document>>|<tree|<with|mode|text|<markup|TeXmacs>>|<with|mode|text|<TeXmacs-version>>>|<tree|<with|mode|text|<markup|style>>|article>|<tree|<with|mode|text|<markup|body>>|<tree|<with|mode|text|<markup|document>>|hello
      world!>>>
    </equation*>
  </example>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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