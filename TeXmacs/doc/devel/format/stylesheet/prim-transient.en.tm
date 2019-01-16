<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Transient markup>

  The tags described in this section are used to control the rendering of
  style files and style file elements. It both contains markup for activation
  and deactivation of content and for the rendering of tags.

  <\explain>
    <explain-macro|active|content>

    <explain-macro|active*|content>

    <explain-macro|inactive|content>

    <explain-macro|inactive*|content><explain-synopsis|activation/deactivation
    of content>
  <|explain>
    These tags can be used to temporarily or permanently change the
    <em|activity> of the <src-arg|content>. In usual documents, tags are by
    default active. In style files, they are by default inactive. For
    instance, an activated fraction is rendered as <frac|1|2>; when
    deactivated, it is rendered as <inactive*|<frac|1|2>>.

    The <markup|active> and <markup|inactive> tags only activate or
    deactivate the root tag of the <src-arg|content>. Typically, a tag which
    contains hidden information (like <markup|hlink>) can be deactivated by
    positioning the cursor just behind it and pressing <key|backspace>. This
    action just deactivates the hyperlink, but not the potentially
    complicated body of the hyperlink. Therefore, the hyperlink is
    transformed into an inactive tag of the form
    <explain-macro|inactive|<with|font-shape|right|<explain-macro|hlink|body|ref>>>.

    The <markup|active*> and <markup|inactive*> variants are used to activate
    or deactivate the whole <src-arg|content> (except when other
    (dis-)activation tags are found inside the <src-arg|content>). The
    <markup|inactive*> is used frequently inside the present documentation in
    order to show the inactive representation of <TeXmacs> content.
    Nevertheless, it is sometimes desirable to reactivate certain subtrees
    inside deactivated content. For instance, the following piece of
    deactivated code (using <markup|disactive*>) contains the reactivated
    subexpression <math|<with|color|red|\<heartsuit\>\<heartsuit\>\<heartsuit\>>>
    (using <markup|active*>):

    <\tm-fragment>
      <inactive*|<assign|love|<macro|from|<active*|<math|<with|color|red|\<heartsuit\>\<heartsuit\>\<heartsuit\>>>>
      from <arg|from>.>>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|inline-tag|name|arg-1|<math|\<cdots\>>|arg-n><explain-synopsis|rendering
    of inline tags>
  <|explain>
    This tag is used for the default inline rendering of an inactive tag with
    a given <src-arg|name> and arguments <src-arg|arg-1> until
    <src-arg|arg-n>. For instance, <inactive*|<inline-tag|foo|x|y>> produces
    <inline-tag|foo|x|y>. The style of the rendering may be customized in the
    <menu|Document|Source|Source tags> menu, or by modifying the
    <src-var|src-style>, <src-var|src-special>, <src-var|src-compact> and
    <src-var|src-close> environment variables.
  </explain>

  <\explain>
    <explain-macro|open-tag|name|arg-1|<math|\<cdots\>>|arg-n>

    <explain-macro|middle-tag|name|arg-1|<math|\<cdots\>>|arg-n>

    <explain-macro|close-tag|name|arg-1|<math|\<cdots\>>|arg-n><explain-synopsis|rendering
    of multi-line tags>
  <|explain>
    These tags are similar to <markup|inline-tag>, when some of the arguments
    of the tag run over several lines. Typical HTML-like tags would
    correspond to <explain-macro|open-tag|name> and
    <explain-macro|close-tag|name>. Since <TeXmacs> macros may take more than
    one argument, a <markup|middle-tag> is provided for separating distinct
    multi-paragraph arguments. Moreover, the opening, middle and closing tags
    may take additional inline arguments for rendering in a compact fashion.
    For instance, the code

    <\tm-fragment>
      <\inactive*>
        <open-tag|theorem>

        <indent|The weather should be nice today.>

        <close-tag|theorem>
      </inactive*>
    </tm-fragment>

    is rendered by default as

    <\tm-fragment>
      <open-tag|theorem>

      <indent|The weather should be nice today.>

      <close-tag|theorem>
    </tm-fragment>

    The rendering may be customized in a similar way as in the case of
    <markup|inline-tag>.
  </explain>

  <\explain>
    <explain-macro|style-with|var-1|val-1|<math|\<cdots\>>|var-n|val-n|body>

    <explain-macro|style-with*|var-1|val-1|<math|\<cdots\>>|var-n|val-n|body><explain-synopsis|alter
    presentation in style files only>
  <|explain>
    This tag may be used in order to temporarily modify the rendering of
    inactive tags, by setting each environment variable <src-arg|var-i> to
    <src-arg|val-i> in the local typesetting context of <src-arg|body>. When
    importing a style file, each <markup|style-with>/<markup|style-with*> tag
    is replaced by its <src-arg|body>. In the case of <markup|style-with>,
    the modified rendering is only applied to the root tag of the
    <src-arg|body>. In the case of <markup|style-with*>, the rendering is
    modified for the entire <src-arg|body>.
  </explain>

  <\explain>
    <explain-macro|style-only|<with|font-shape|right|<explain-macro|foo|content>>>

    <explain-macro|style-only*|<with|font-shape|right|<explain-macro|foo|content>>><explain-synopsis|content
    for use in style files only>
  <|explain>
    This tag may be used in order to render an inactive tags as whether we
    applied the macro <markup|foo> on it. When importing a style file, each
    <markup|style-only>/<markup|style-only*> tag is replaced by its
    <src-arg|content>. In the case of <markup|style-only>, the modified
    rendering is only applied to the root tag of the <src-arg|content>. In
    the case of <markup|style-only*>, the rendering is modified for the
    entire <src-arg|content>.
  </explain>

  <\explain>
    <explain-macro|symbol|symbol>

    <explain-macro|latex|cmd>

    <explain-macro|hybrid|cmd>

    <explain-macro|hybrid|cmd|arg><explain-synopsis|auxiliary tags for
    entering special content>
  <|explain>
    These tags are used only temporarily when entering special content.

    When pressing <shortcut|(make 'symbol)>, a <markup|symbol> tag is
    created. After entering the name of the symbol, or the ASCII-code of the
    symbol and pressing return, the <markup|symbol> tag is replaced by the
    corresponding symbol (usually a string enclosed in
    <verbatim|\<less\>\<gtr\>>).

    When pressing <key|\\>, a <markup|hybrid> tag is created. After entering
    a string and pressing return, it is determined whether the string
    corresponds to a <LaTeX> command, a macro argument, a macro or an
    environment variable (in this order). If so, then the <markup|hybrid> tag
    is replaced by the appropriate content. When pressing <key|\\> while a
    selection is active, then the selection automatically becomes the
    argument of the hybrid command (or the hybrid command itself, when
    recognized).

    The <markup|latex> tag behaves similarly as the <markup|hybrid> tag
    except that it only recognizes <LaTeX> commands.
  </explain>

  The rendering macros for source trees are built-in into <TeXmacs>. They
  should not really be considered as primitives, but they are not part of any
  style file either.

  <\explain>
    <explain-macro|indent|body><explain-synopsis|indent some content>
  <|explain>
    Typeset the <src-arg|body> using some indentation.
  </explain>

  <\explain>
    <explain-macro|rightflush><explain-synopsis|indent some content>
  <|explain>
    Flush to the right. This macro is useful to make the end of a block
    environment run until the right margin. This allows for more natural
    cursor positioning and a better layout of the informative boxes.
  </explain>

  <\explain>
    <explain-macro|src-macro|macro-name>

    <explain-macro|src-var|variable-name>

    <explain-macro|src-arg|argument-name>

    <explain-macro|src-tt|verbatim-content>

    <explain-macro|src-integer|interger>

    <explain-macro|src-length|length>

    <explain-macro|src-error|message><explain-synopsis|syntactic highlighting
    on purpose>
  <|explain>
    These macros are used for the syntactic highlighting of source trees.
    They determine how to render subtrees which correspond to macro names,
    variable names, argument names, verbatim content, integers, lengths and
    error messages.
  </explain>

  <\explain>
    <explain-macro|src-title|title>

    <explain-macro|src-style-file|name|version>

    <explain-macro|src-package|name|version>

    <explain-macro|src-package-dtd|name|version|dtd|dtd-version><explain-synopsis|style
    and package administration>
  <|explain>
    These macros are used for the identification of style files and packages
    and their corresponding <abbr|D.T.D.>s. The <markup|src-title> is a
    container for <markup|src-style-file>, <markup|src-package>,
    <markup|src-package-dtd> as well as <markup|src-license> and
    <markup|src-copyright> macros.

    The <markup|src-style-file> tag specifies the <src-arg|name> and
    <src-arg|version> of a style file and sets the environment variable with
    <src-var|<src-arg|name>-style> to <src-arg|version>. The
    <markup|src-package-dtd> specifies the <src-arg|name> and
    <src-arg|version> of a package, as well as the corresponding
    <src-arg|dtd> and its version <src-arg|dtd-version>. It sets the
    environment variable <src-var|<src-arg|name>-package> to
    <src-arg|version> and <src-var|<src-arg|dtd>-dtd> to
    <src-arg|dtd-version>. The <markup|src-package> tag is a shorthand for
    <markup|src-package-dtd> when the name of the <abbr|D.T.D.> coincides
    with the name of the package.
  </explain>

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