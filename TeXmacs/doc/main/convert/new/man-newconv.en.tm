<TeXmacs|1.99.11>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Adding new data formats and converters>

  Using the <name|Guile>/<name|Scheme> extension language, it is possible to
  add new data formats and converters to <TeXmacs> in a modular way. Usually,
  the additional formats and converters are declared in your personal
  <verbatim|~/.TeXmacs/progs/my-init-texmacs.scm> or a dedicated plug-in.
  Some examples may be found in the directory
  <verbatim|$TEXMACS_PATH/progs/convert>, like
  <hlink|<verbatim|init-html.scm>|$TEXMACS_PATH/progs/convert/html/init-html.scm>.

  <paragraph*|Declaring new formats>

  A new format is declared using the command

  <\scm-code>
    (define-format <em|format>

    \ \ (:name <em|format-name>)

    \ \ <em|options>)
  </scm-code>

  Here <verbatim|<em|format>> is a symbol which stands for the format and
  <verbatim|<em|format-name>> a string which can be used in menus. In fact, a
  data format usually comes in several variants: a format
  <verbatim|<em|format>-file> for files, a format
  <verbatim|<em|format>-document> for entire documents, a format
  <verbatim|<em|format>-snippet> for snippets, like selections, and
  <verbatim|<em|format>-object> for the preferred internal scheme
  representation for doing conversions (<abbr|i.e.> the parsed variant of the
  format). Converters from <verbatim|<em|format>-file> to
  <verbatim|<em|format>-document> and <em|vice versa> are provided
  automatically.

  The user may specify additional options for the automatic recognition of
  formats by their file suffix and contents. The possible suffixes for a
  format, with the default one listed first, may be specified using

  <\scm-code>
    (:suffix <em|default-suffix> <em|other-suffix-1> ... <em|other-suffix-n>)
  </scm-code>

  A (heuristic) routine for recognizing whether a given document matches the
  format can be specified using either one of the following:

  <\scm-code>
    (:recognize <em|predicate>)

    (:must-recognize <em|predicate>)
  </scm-code>

  In the first case, suffix recognition takes precedence over document
  recognition and in the second case, the heuristic recognition is entirely
  determined by the document recognition predicate.

  A format can be removed from menus using the following:

  <\scm-code>
    (:hidden)
  </scm-code>

  <paragraph*|Declaring new converters>

  New converters are declared using

  <\scm-code>
    (converter <em|from> <em|to>

    \ \ <em|options>)
  </scm-code>

  The actual converter is specified using either one of the following
  options:

  <\scm-code>
    (:function <em|converter>)

    (:function-with-options <em|converter-with-options>)

    (:shell <em|prog> <em|prog-pre-args> from <em|progs-infix-args> to
    <em|prog-post-args>)
  </scm-code>

  In the first case, the <verbatim|<em|converter>> is a routine which takes
  an object of the <verbatim|<em|from>> format and returns a routine of the
  <verbatim|<em|to>> format. In the second case, the
  <verbatim|<em|converter>> takes an additional association list as its
  second argument with options for the converter. In the last case, a shell
  command is specified in order to convert between two file formats. The
  converter is activated only then, when <verbatim|<em|prog>> is indeed found
  in the path. Also, auxiliary files may be created and destroyed
  automatically.

  <TeXmacs> automatically computes the transitive closure of all converters
  using a shortest path algorithm. In other words, if you have a converter
  from <math|x> to <math|y> and a converter from <math|y> to <math|z>, then
  you will automatically have a converter from <math|x> to <math|z>. A
  \Pdistance between two formats via a given converter\Q may be specified
  using

  <\scm-code>
    (:penalty <em|floating-point-distance>)
  </scm-code>

  Further options for converters are:

  <\scm-code>
    (:require <em|cond>)

    (:option <em|option> <em|default-value>)
  </scm-code>

  The first option specifies a condition which must be satisfied for this
  converter to be used. This option should be specified as the first or
  second option and always after the <verbatim|:penalty> option. The
  <verbatim|:option> option specifies an option for the converter with its
  default value. This option automatically become a user preference and it
  will be passed to all converters with options.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>