<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Linking primitives>

  <\big-table>
    <tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|label>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|reference>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|pageref>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|hlink>>|<cell|<with|mode|math|2>>|<cell|Yes>|<cell|First>|<cell|Typesetting>|<cell|Logical>>|<row|<cell|<markup|include>>|<cell|1>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Logical>>>>>
  </big-table|Linking primitives>

  <\explain>
    <explain-macro|label|name><explain-synopsis|reference target>
  <|explain>
    The operand must evaluate to a literal string, it is used as a target
    name which can be referred to by <markup|reference>, <markup|pageref> and
    <markup|hlink> tags.

    Label names should be unique in a document and in a project.

    Examples in this section will make references to an example
    <markup|label> named ``there''.

    <\tm-fragment>
      <inactive*|<label|there>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|reference|name><explain-synopsis|reference to a name>
  <|explain>
    The operand must evaluate to a literal string, which is the name of a
    <markup|label> defined in the current document or in another document of
    the current project.

    <\tm-fragment>
      <inactive*|<reference|there>>
    </tm-fragment>

    The <markup|reference> is typeset as the value of the variable
    <src-var|the-label> at the point of the target <markup|label>. The
    <src-var|the-label> variable is set by many numbered structures:
    sections, figures, numbered equations, <no-break>etc.

    A <markup|reference> reacts to mouse clicks as an hyperlink.
  </explain>

  <\explain>
    <explain-macro|pageref|name><explain-synopsis|page reference to a name>
  <|explain>
    The operand must evaluate to a literal string, which is the name of a
    <markup|label> defined in the current document or in another document of
    the current project.

    <\tm-fragment>
      <inactive*|<pageref|there>>
    </tm-fragment>

    The <markup|><markup|pageref> is typeset as the number of the page
    containing the target <markup|label>. Note that page numbers are only
    computed when the document is typeset with page-breaking, that is not in
    ``automatic'' or ``papyrus'' page type.

    A <markup|pageref> reacts to mouse clicks as an hyperlink.
  </explain>

  <\explain>
    <explain-macro|hlink|content|url><explain-synopsis|inline hyperlink>
  <|explain>
    This primitive produces an hyperlink with the visible text
    <src-arg|content> pointing to <src-arg|url>. The <src-arg|content> is
    typeset as inline <src-arg|url>. The <src-arg|url> must evaluate to a
    literal string in <abbr|URL> syntax and can point to local or remote
    documents, positions inside documents can be be specified with labels.

    The following examples are typeset as hyperlinks pointing to the label
    ``there'', respectively in the same document, in a document in the same
    directory, and on the web.

    <\tm-fragment>
      <inactive*|<hlink|same document|#there>>

      <inactive*|<hlink|same directory|file.tm#there>>

      <inactive*|<hlink|on the web|http://example.org/#there>>
    </tm-fragment>

    If the document is not editable, the hyperlink is traversed by a simple
    click, if the document is editable, a double-click is required.
  </explain>

  <\explain>
    <explain-macro|include|url><explain-synopsis|include another document>
  <|explain>
    The operand must be a literal string and is interpreted as a file name.
    The content of this file is typeset in place of the <markup|include> tag,
    which must be placed in <re-index|block context>.
  </explain>

  <tmdoc-copyright|2004|David Allouche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>