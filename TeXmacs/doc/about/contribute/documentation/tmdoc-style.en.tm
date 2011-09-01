<TeXmacs|1.0.7.12>

<style|tmdoc>

<\body>
  <tmdoc-title|Using the tmdoc style>

  Besides the <hlink|copyright information|copyright.en.tm> macros and
  <hlink|traversal macros|traversal.en.tm>, which have been documented
  before, the <tmstyle|tmdoc> style comes with a certain number of other
  macros and functions, which you should use whenever appropriate:

  <\description>
    <item*|<markup|shortcut>>This macro is used to indicate a keyboard
    shortcut for a <scheme> command. For instance, the shortcut for
    <scm|(new-buffer)> is <shortcut|(new-buffer)>.

    <item*|<markup|key>>This unary macro is used for explicit keyboard input.
    For instance, when giving <rigid|<verbatim|A C-b return>> as argument,
    the result is <key|A C-b return>.

    <item*|<markup|menu>>This function with an arbitrary number of arguments
    indicates a menu like <menu|File> or <menu|Document|Language>. Menu
    entries are automatically translated by this function.

    <item*|<markup|markup>>This macro is used in order to indicate a macro or
    a function like <markup|section>.

    <item*|<markup|tmstyle>>This macro indicates the name of a <TeXmacs>
    style file or package like <tmstyle|article>.

    <item*|<markup|tmpackage>>This macro indicates the name of a <TeXmacs>
    package like <tmpackage|std-markup>.

    <item*|<markup|tmdtd>>This macro indicates the name of a <TeXmacs>
    <abbr|d.t.d.> like <tmdtd|number-env>.
  </description>

  Notice that the contents of none of the above tags should be translated
  into foreign languages. Indeed, for menu tags, the translations are done
  automatically, so as to keep the translations synchronized with the
  translations of the actual <TeXmacs> menus. In the cases of markup, styles,
  packages and <abbr|d.t.d.>s, it is important to keep the original name,
  because it often corresponds to a file name.

  The following macros and functions are used for linking and indexing
  purposes, although they should be improved in the future:

  <\description>
    <item*|<markup|simple-link>>This macro takes an URL <math|x> as argument
    and is a hyperlink with name and destination <math|x>.

    <item*|<markup|hyper-link>>This macro is a usual hyperlink.

    <item*|<markup|concept-link>>This macro takes a concept as argument.
    Later on an appropriate hyperlink might be created automatically from
    this and the other documentation.

    <item*|<markup|only-index>>Index a simple string.

    <item*|<markup|def-index>>Definition of a new concept; the text is
    printed in italic and indexed.

    <item*|<markup|re-index>>Reappearance of an already defined concept; the
    text is printed in roman and put in the index.
  </description>

  The following tags are also frequently used:

  <\description>
    <item*|<markup|scheme>>The <scheme> language.

    <item*|<markup|c++>>The <c++> language.

    <item*|<markup|framed-fragment>>For displaying a piece of code in a nice
    frame.

    <item*|<markup|tm-fragment>>Single out a fragment of a <TeXmacs>
    document.

    <item*|<markup|scm-code>>For multi-paragraph <scheme> code.

    <item*|<markup|cpp-code>>For multi-paragraph <c++> code.

    <item*|<markup|scm>>For a short piece of <scheme> code.

    <item*|<markup|cpp>>For a short piece of <c++> code.

    <item*|<markup|descriptive-table>>For descriptive tables; such tables can
    be used to document lists of keyboard shortcuts, different types of
    markup, <abbr|etc.>
  </description>

  The <tmstyle|tmdoc> style inherits from the <tmstyle|generic> style and you
  should use macros like <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> from this style whenever appropriate.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven>

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