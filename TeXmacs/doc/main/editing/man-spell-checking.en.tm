<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Spell checking>

  If the program <verbatim|ispell> has been installed on your system, then
  you may use it to check your text for misspelled words by pressing
  <shortcut|(spell-start)> or <submenu|Edit|spell>. Notice that you might have to
  verify that the dictionaries corresponding to the languages in which your
  texts have been written have been installed on your system; this is usually
  the case for English.

  When you launch the spell checker (either on the whole text or a selected
  region), you will be prompted at each misspelled word and the footer
  displays the available options:

  <\description>
    <expand|item*|a)>Accepts the misspelled word and all its future
    occurrences in the text.

    <expand|item*|r)>Replace the misspelled word by a correction you have to
    enter.

    <expand|item*|i)>Indicate that the ``misspelled'' word is actually
    correct and that it has to be inserted in your personal dictionary.

    <expand|item*|1-9)>Several suggested corrections for your misspelled
    word.
  </description>

  Notice that <verbatim|ispell> just checks for misspelled words. No
  grammatical faults will be detected.

  When starting the spell checker, it will use the dictionary of the language
  which is active at the current cursor position (or the start of a
  selection). Only text in that language will be checked for. If your
  document contains text in several languages, then you will have to launch
  the spell checker once for each language being used.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
