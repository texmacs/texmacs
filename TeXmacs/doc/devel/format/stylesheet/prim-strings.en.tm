<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Operations on text>

  <\explain>
    <explain-macro|length|expr><explain-synopsis|length of a string>
  <|explain>
    If <src-arg|expr> is a string, the length of the string is returned. For
    instance, <inactive*|<length|Hello>> evaluates to <length|Hello>.
  </explain>

  <\explain>
    <explain-macro|range|expr|start|end><explain-synopsis|extract a
    substring>
  <|explain>
    Return the substring of <src-arg|expr> starting at position
    <src-arg|start> and ending at position <src-arg|end> (not included). For
    instance, <inactive*|<range|hottentottententententoonstelling|9|15>>
    evaluates to <range|hottentottententententoonstelling|9|15>.
  </explain>

  <\explain>
    <explain-macro|merge|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|concatenate
    strings>
  <|explain>
    This primitive may be used to concatenate several strings
    <src-arg|expr-1> until <src-arg|expr-n>. For instance,
    <inactive*|<merge|Hello|World>> produces <merge|Hello|World>.
  </explain>

  <\explain>
    <explain-macro|number|number|render-as><explain-synopsis|alternative
    rendering of numbers>
  <|explain>
    Renders a <src-arg|number> in a specified way. Supported values for
    <src-arg|render-as> are

    <\description>
      <item*|roman>Lower case Roman: <inactive*|<number|18|roman>>
      <with|mode|math|\<longrightarrow\>> <number|18|roman>.

      <item*|Roman>Upper case Roman: <inactive*|<number|18|Roman>>
      <with|mode|math|\<longrightarrow\>> <number|18|Roman>.

      <item*|alpha>Lower case letters: <inactive*|<number|18|alpha>>
      <with|mode|math|\<longrightarrow\>> <number|18|alpha>.

      <item*|Alpha>Upper case letters: <inactive*|<number|18|Alpha>>
      <with|mode|math|\<longrightarrow\>> <number|18|Alpha>.
    </description>
  </explain>

  <\explain>
    <explain-macro|date>

    <explain-macro|date|format>

    <explain-macro|date|format|language><explain-synopsis|obtain the current
    date>
  <|explain>
    Returns the current date in a specified <src-arg|format> (which defaults
    to a standard language-specific format when empty) and a specified
    <src-arg|language> (which defaults to English). The format is similar to
    the one used by the <name|Unix> <verbatim|date> command. For instance,
    <inactive*|<date>> evaluates to ``<date>'', <inactive*|<date||french>> to
    ``<date||french>'' and <inactive*|<date|%d %B om %k:%M|dutch>> to
    ``<date|%d %B om %k:%M|dutch>''.
  </explain>

  <\explain>
    <explain-macro|translate|what|from|into><explain-synopsis|translation of
    strings>
  <|explain>
    Returns the translation of a string <src-arg|what> of the language
    <src-arg|from> into the language <src-arg|into>, using the built-in
    <TeXmacs> dictionaries. The languages should be specified in lowercase
    letters. For instance, <inactive*|<translate|File|english|french>> yields
    ``<translate|File|english|french>''.

    The list of currently available languages can be checked in the
    <menu|Document|Language> menu. The built-in <TeXmacs> dictionaries can be
    found in

    <verbatim| \ \ \ $TEXMACS_PATH/languages/natural/dic>

    When attempting to use a non-existing dictionary, the program may quit.
    For most purposes, it is more convenient to use the <markup|localize>
    <verbatim|>macro, which converts a string from English into the current
    language.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>