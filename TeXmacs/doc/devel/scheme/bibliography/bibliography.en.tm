<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Writing <TeXmacs> bibliography styles>

  <section|Introduction>

  <TeXmacs> admits support both for <BibTeX> and a native tool for managing
  bibliographies. <BibTeX> styles are denoted by their usual names. <TeXmacs>
  styles are prefixed by <verbatim|tm->. For example, the <TeXmacs>
  <verbatim|<rigid|tm-plain>> style is the replacement for the <BibTeX>
  <verbatim|plain> style. Equivalents for the following <BibTeX> styles have
  been implemented: <verbatim|abbrv>, <verbatim|alpha>, <verbatim|ieeetr>,
  <verbatim|plain> et <verbatim|siam>. These styles can therefore be used
  without installation of <BibTeX>.

  New bibliography styles can be defined by the user. Each style is
  associated to a<nbsp>unique <scheme> file, which should be added to the
  directory <verbatim|$TEXMACS_PATH/prog/bibtex>. Style files are treated as
  regular Scheme programs. Since the creation of a style file from scratch is
  a complex task, we recommend you customize existing style files or modules.
  In the next sections, we will describe the creation of a new style on a
  simple example and give a detailed lists of available <scheme> functions
  which facilitate the creation of new styles.

  <section|Example of a simple bibliography style>

  Bibliographic style files are stored in directory
  <verbatim|$TEXMACS_PATH/progs/bibtex>. They have the name of the style
  followed with extension <verbatim|.scm>. For example,
  <verbatim|example.scm> is the file name associated to the style
  <verbatim|example>, which is denoted by <verbatim|tm-example> when it is
  used in a <TeXmacs> document.

  All style files must be declared as a module as follows:

  <\scm-code>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils)))
  </scm-code>

  The module <verbatim|bib-utils> contains all useful functions needed to
  write bibliographic styles.

  All style files must me declared as a bibliographic style as follows:

  <scm-code|(bib-define-style "example" "plain")>

  The first argument to <scm|bib-define-style> is the name of the current
  style. The second argument is the name of a fall-back style,
  <verbatim|plain> in our case. If a function is not defined in current
  style, the function from the fall-back style is used instead. Hence, the
  following minimalistic style file behaves in an identical way as the
  <verbatim|plain> style:

  <\scm-code>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils)))

    \;

    (bib-define-style "example" "plain")
  </scm-code>

  Each formatting function defined in the default style can be overloaded in
  the current style. For example, the function <scm|bib-format-date> is used
  to format the date in the <verbatim|plain> style. It is redefinable in our
  example style as follows:

  <\scm-code>
    (tm-define (bib-format-date e)

    \ \ (:mode bib-example?)

    \ \ (bib-format-field e "year"))
  </scm-code>

  All exported functions must be prefixed with <verbatim|bib->. Overloaded
  functions must be followed with directive <scm|(:mode bib-example?)>, in
  which <verbatim|example> is the name of the current style.

  Our complete example file <verbatim|example.scm> is as follows:

  <\scm-code>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils)))

    \;

    (bib-define-style "example" "plain")

    \;

    (tm-define (bib-format-date e)

    \ \ (:mode bib-example?)

    \ \ (bib-format-field e "year"))
  </scm-code>

  It behaves in a similar way as the <verbatim|plain> style, except that all
  dates are formatted according to our custom routine.

  <section|<scheme> functions for writing bibliography styles>

  <subsection|Style management>

  <\explain>
    <scm|(bib-define-style name default)><explain-synopsis|style declaration>
  <|explain>
    This function declares a style called <scm|name> (string) with fall-back
    style <scm|default> (string). The style is selected by choosing
    <verbatim|tm-><scm|name> when adding a bibliography to a document.
    Whenever a<nbsp>formatting function is not defined in the current style,
    its definition in the fall-back style is used as replacement.
  </explain>

  <\explain>
    <scm|(bib-with-style style expr)><explain-synopsis|local style>
  <|explain>
    This function evaluates expression <scm|expr> as if the current style
    were <scm|style> (string).
  </explain>

  <subsection|Field related routines>

  <\explain>
    <scm|(bib-field entry field)><explain-synopsis|field data>
  <|explain>
    This function creates a <TeXmacs> tree corresponding to the field
    <scm|field> (string) of entry <scm|entry> without format. In some cases,
    the output is special:

    <\itemize-dot>
      <item>If <scm|field> is <scm|"author"> or <scm|"editor">, we return a
      tree with label <verbatim|bib-names> followed by a list of author
      names; each author name is a tree with label <verbatim|bib-name>
      containing four elements: first name, particule (von), last name and
      suffix (jr);

      <item>If <scm|field> is <scm|"page">, then we return a list of
      integers: the empty list, or a singleton with a page number, or a pair
      corresponding to a pages interval.
    </itemize-dot>
  </explain>

  <\explain>
    <scm|(bib-format-field entry field)><explain-synopsis|basic format>
  <|explain>
    This function creates a <TeXmacs> tree corresponding to the field
    <scm|field> (string) of entry <scm|entry>, with basic format.
  </explain>

  <\explain>
    <scm|(bib-format-field-Locase entry field)><explain-synopsis|special
    format>
  <|explain>
    This function is similar to <scm|bib-format-field> ; but field are
    formatted in lower case with an upper case letter at the beginning.
  </explain>

  <\explain>
    <scm|(bib-empty? entry field)><explain-synopsis|null-test of a field>
  <|explain>
    This function returns boolean <scm|#t> if the field <scm|field> (string)
    of entry <scm|entry> is empty or absent; it returns <scm|#f> in the other
    cases.
  </explain>

  <subsection|Routines for structuring the output>

  <\explain>
    <scm|(bib-new-block tm)><explain-synopsis|new block>
  <|explain>
    This function creates a<TeXmacs> tree consisting of a block containing
    <TeXmacs> tree <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-new-list sep ltm)><explain-synopsis|separated list>
  <|explain>
    This function creates a <TeXmacs> tree which is the concatenation of all
    the elements of list <scm|ltm> separated with <TeXmacs> tree <scm|sep>.
  </explain>

  <\explain>
    <scm|(bib-new-list-spc ltm)><explain-synopsis|blank separated list>
  <|explain>
    This function is equivalent to the evaluation of <scm|(bib-new-list " "
    ltm)>.
  </explain>

  <\explain>
    <scm|(bib-new-sentence ltm)><explain-synopsis|new sentence>
  <|explain>
    This function creates a <TeXmacs> tree corresponding to a sentence
    containing all the elements of list <scm|ltm> separated by commas.
  </explain>

  <subsection|Routines for textual manipulations>

  <\explain>
    <scm|(bib-abbreviate name dot spc)><explain-synopsis|name abbreviation>
  <|explain>
    This function creates a <TeXmacs> tree corresponding to the abbreviation
    of the name contained in <scm|name> <TeXmacs> tree: it retrieves the list
    of first letters of each word, followed by <scm|dot> (<TeXmacs> tree) and
    separated by <scm|spc> (<TeXmacs> tree).
  </explain>

  <\explain>
    <scm|(bib-add-period tm)><explain-synopsis|dot>
  <|explain>
    This function creates a <TeXmacs> tree with a dot at the end of <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-default tm)><explain-synopsis|default <TeXmacs> tree>
  <|explain>
    This function creates a <TeXmacs> tree without label
    <verbatim|keep-case>.
  </explain>

  <\explain>
    <scm|(bib-emphasize tm)><explain-synopsis|italic>
  <|explain>
    This function creates a <TeXmacs> tree corresponding to the italic
    version of <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-locase tm)><explain-synopsis|lower case>
  <|explain>
    This function creates a <TeXmacs> tree, which is equal to <scm|tm> with
    all letters in lower case, except for those within <verbatim|keep-case>
    blocks.
  </explain>

  <\explain>
    <scm|(bib-prefix tm nbcar)><explain-synopsis|beginning of a <TeXmacs>
    tree>
  <|explain>
    This function returns a string containing the first <scm|nbcar>
    characters of <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-upcase tm)><explain-synopsis|upper case>
  <|explain>
    This function creates a <TeXmacs> tree, which is equal to <scm|tm> with
    all letters in upper case, except for those within <verbatim|keep-case>
    blocks.
  </explain>

  <\explain>
    <scm|(bib-upcase-first tm)><explain-synopsis|upper case first letter>
  <|explain>
    This function creates a <TeXmacs> tree, which is equal to <scm|tm> with
    its first letter in upper case, except inside <verbatim|keep-case>
    blocks.
  </explain>

  <subsection|Miscellaneous routines>

  <\explain>
    <scm|(bib-null? v)><explain-synopsis|null-test>
  <|explain>
    This function returns boolean <scm|#t> if value <scm|v> is empty; it
    returns <scm|#f> in the other cases.
  </explain>

  <\explain>
    <scm|(bib-purify tm)><explain-synopsis|flattening of a <TeXmacs> tree>
  <|explain>
    This function returns a string made of all letters of the <TeXmacs> tree
    <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-simplify tm)><explain-synopsis|simplification of a <TeXmacs>
    tree>
  <|explain>
    This function returns a <TeXmacs> tree corresponding to the
    simplification of <TeXmacs> tree <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-text-length tm)><explain-synopsis|length of a <TeXmacs> tree>
  <|explain>
    This function returns the length of <TeXmacs> tree <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-translate msg)><explain-synopsis|translation>
  <|explain>
    This function translates the string message <scm|msg> from english into
    the current language.
  </explain>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>