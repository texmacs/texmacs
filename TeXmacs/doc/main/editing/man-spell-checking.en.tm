<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Spell checking>

  If the program <verbatim|ispell> has been installed on your system, then
  you may use it to check your text for misspelled words by pressing
  <shortcut|(interactive-spell)> or <menu|Edit|Spell>. Notice that you might
  have to verify that the dictionaries corresponding to the languages in
  which your texts have been written have been installed on your system; this
  is usually the case for English.

  When you launch the spell checker (either on the whole text or on a
  selected region), you will be prompted at each misspelled word and the
  footer displays the available options:

  <\description>
    <item*|a)>Accepts the misspelled word and all its future occurrences in
    the text.

    <item*|r)>Replace the misspelled word by a correction that you have to
    enter.

    <item*|i)>Indicate that the \Pmisspelled\Q word is actually correct and
    that it has to be inserted in your personal dictionary.

    <item*|1-9)>Several suggested corrections for your misspelled word.
  </description>

  Notice that <verbatim|ispell> just checks for misspelled words. No
  grammatical faults will be detected.

  When starting the spell checker, it will use the dictionary of the language
  that is active at the current cursor position (or the start of a
  selection). Only text in that language will be checked. If your document
  contains text in several languages, then you will have to launch the spell
  checker once for each language being used.

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>