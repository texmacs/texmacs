<TeXmacs|1.0.7.19>

<style|tmdoc>

<\body>
  <tmdoc-title|General environment variables>

  <\explain>
    <var-val|mode|text><explain-synopsis|major mode>
  <|explain>
    This very important environment variable determines the <em|current
    mode>. There are four possible values: <verbatim|text> (text mode),
    <verbatim|math> (mathematical mode), <verbatim|prog> (programming mode)
    and <verbatim|src> (source mode). The behaviour of the editor (menus,
    keystrokes, typesetting, <abbr|etc.>) depends heavily on the mode. For
    example, the following code may be used in order to include a
    mathematical formula inside text:

    <\tm-fragment>
      The formula <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> is well known.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|The formula <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> is well
      known.>
    </tm-fragment>

    Some other environment variables (mainly the language and the font) also
    depend on the current mode (in this context, the source mode always
    behaves in a similar way as the text mode). During
    copy<space|0.2spc>&<space|0.2spc>paste and
    search<space|0.2spc>&<space|0.2spc>replace operations, <TeXmacs> tries to
    preserve the mode.
  </explain>

  <\explain>
    <var-val|language|english>

    <var-val|math-language|texmath>

    <var-val|prog-language|scheme><explain-synopsis|language>
  <|explain>
    A second major environment variable is the <em|current language>. In
    fact, there are three such environment variables: one for each mode. The
    language in which content is written is responsible for associating a
    precise semantics to the content. This semantics is used for different
    purposes:

    <\itemize>
      <item>The language may specify rules for typesetting content. For
      instance, the text language specifies punctuation and hyphenation
      rules. Similarly the mathematical language containns spacing
      information for mathematical operators.

      <item>Several editing operations depend on the current language: when
      performing a search or replace operation, <TeXmacs> is both mode and
      language sensitive. Similarly, the text language determines the
      dictionary to use when spell-checking the document.

      <item>The language controls (among other parameters like the mode and
      the document format) the way content is being converted from one
      context to another.

      Currently, no real language-dependent conversions have been implemented
      yet. But in the future one may imagine that copying a piece of English
      text to a document written in French will perform an automatic
      translation. Similarly, a mathematical document might be converted from
      infix to postfix notation.

      <item>The programming language determines the current scripting
      language in use. Other scripting languages than <scheme> are currently
      only used for interactive sessions, but primitives like <markup|extern>
      might become language-sensitive in the future.
    </itemize>

    At the moment, the current language is mainly used as a hint for
    indicating the semantics of text: it is not required that a text written
    in English contains no spelling errors, or that a formula written in a
    mathematical language is mathematically or even syntactically correct.
    Nevertheless, the editor is intended to enforce correctness more and
    more, especially for mathematics.

    The language may be specified globally for the whole document in
    <menu|Document|Language> and locally for a piece of text in
    <menu|Format|Language>.
  </explain>

  <\explain>
    <var-val|prog-session|default><explain-synopsis|name of programming
    session>
  <|explain>
    This environment variables is used in addition to the
    <src-var|prog-language> variable in order to determine a concrete
    implementation as well as a particular instance of the current
    programming language. For instance, in case of the
    <hlink|<name|Maxima>|../../../../plugins/maxima/doc/maxima-abstract.en.tm>
    language, different implementation may be used fooor the underlying
    <name|Lisp>. Similarly, one may wish to run two different instances of
    <name|Maxima> in parallel.
  </explain>

  <\explain>
    <label|magnification><var-val|magnification|1><explain-synopsis|magnification>
  <|explain>
    This variable determines the magnification which is applied to all
    content. Magnifications bigger than one are typically useful for
    presentations (from slides or from a laptop):

    <\tm-fragment>
      normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>
    </tm-fragment>

    <\tm-fragment>
      <inactive*|normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>>
    </tm-fragment>

    The magnification should not be confused with the <hlink|font
    size|env-font.en.tm#font-base-size>: contrary to the magnification, the
    font size may also affect the shapes of the glyphs. The magnification is
    usually specified for the entire document in
    <menu|Document|Magnification>.
  </explain>

  <\explain>
    <var-val|bg-color|white><explain-synopsis|background color>
  <|explain>
    The background color for your document, as specified in
    <menu|Document|Color|Background>.
  </explain>

  <\explain>
    <var-val|color|black><explain-synopsis|foreground color>
  <|explain>
    The current foreground color of text and graphics, as specified in
    <menu|Document|Color|Foreground> or <menu|Format|Color>. Named colors,
    like ``<verbatim|Salmon>'', are supported for different color charts:
    <hlink|<verbatim|dvips>|https://en.wikibooks.org/wiki/LaTeX/Colors#The_68_standard_colors_known_to_dvips>,
    <hlink|<verbatim|x11>|https://en.wikipedia.org/wiki/X11_color_names#Color_name_charts>
    and <hlink|<verbatim|html>|https://www.w3schools.com/colors/colors_names.asp>.
  </explain>

  <\explain>
    <var-val|preamble|false><explain-synopsis|edit source tree?>
  <|explain>
    This flag determines whether we are editing normal text or a style-sheet.
    The source tree or preamble mode may be selected in
    <menu|Document|Source|Edit source tree>.
  </explain>

  <\explain>
    <var-val|info-flag|short><explain-synopsis|informative flags style>
  <|explain>
    This variable controls the rendering of informative flags, which are for
    instance used to indicate the locations of otherwise invisible labels or
    typesetting directives. The <src-var|info-flag> may take the values
    <verbatim|none>, <verbatim|short> and <verbatim|detailed>:

    <\tm-fragment>
      <with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|<with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.>
    </tm-fragment>

    Usually, the rendering of informative flags is specified document-wide in
    <menu|Document|Informative flags>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>