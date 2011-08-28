<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard utilities>

  Besides the basic concepts from the previous sections, which underly the
  scheme API for <TeXmacs>, the <value|scheme> kernel implements several
  other utilities and language extensions. In this section, we will briefly
  sketch some of them on hand of examples. Further details can be found in
  the chapter about <hlink|<TeXmacs> extensions to <value|scheme> and
  utilities|../utils/scheme-utils.en.tm>.

  <paragraph*|Regular expressions>

  <TeXmacs> implements the routines <scm|match?> and <scm|select> for
  matching regular expressions and selecting subexpressions along a ``path''.
  These routines both work for the <verbatim|content> type. For instance, in
  order to search all expressions of the form

  <\equation*>
    <frac|<with|color|brown|a>|1+<sqrt|<with|color|brown|b>>>
  </equation*>

  in the current buffer, where <with|mode|math|<with|color|brown|a>> and
  <with|mode|math|<with|color|brown|b>> are general expressions, one may use
  the following <value|scheme> command:

  <with|prog-language|scheme|prog-session|default|<\session>
    <\input|scheme] >
      (select (buffer-tree) '(:* (:match (frac :%1 (concat "1+" (sqrt
      :%1))))))
    </input>
  </session>>

  <paragraph*|Dialogues>

  <TeXmacs> supports several commands for asynchronous evaluation of scheme
  commands and interactive dialogues with the user. In general, asynchroneous
  instructions have to be encapsulated inside a ``<scm|dialogue> block''. For
  instance, when executing the following scheme command, you will be prompted
  for two numbers, whose product will be displayed in the footer:

  <\with|prog-language|scheme|prog-session|default>
    <\session>
      <\input|scheme] >
        (dialogue

        \ \ (let* ((a (string-\<gtr\>number (dialogue-ask "First number:")))

        \ \ \ \ \ \ \ \ \ (b (string-\<gtr\>number (dialogue-ask "Second
        number:"))))

        \ \ \ \ (set-message (number-\<gtr\>string (* a b)) "product")))
      </input>
    </session>
  </with>

  <paragraph*|User preferences>

  When writing a plug-in, you may wish to define some new user preferences.
  This can be done using the <scm|define-preferences> command, which adds a
  list of user preferences, together with their default values and a
  call-back routine. The call-back routine is called whenever you change the
  corresponding preference. For instance:

  <\scheme-fragment>
    (define-preferences

    \ \ ("Gnu's hair color" "brown" notify-gnu-hair-change)

    \ \ ("Snail's cruising speed" "1mm/sec" notify-Achilles))
  </scheme-fragment>

  Preferences can be set, reset and read using <scm|set-preference>,
  <scm|reset-preference> and <scm|get-preference>.

  <paragraph*|New data formats and converters>

  New data formats and converters can be declared using the
  <scm|define-format> and <scm|converter> instructions. When a format can be
  converted from or into <TeXmacs>, then it will automatically appear into
  the <menu|File|Export> and <menu|File|Import> menus. Similarly, when a
  format can be converted to <name|Postscript>, then it also becomes a valid
  format for images. <TeXmacs> also attempts to combine explictly declared
  converters into new ones.

  Typically, the declaration of a new format and a converter would look like:

  <\scheme-fragment>
    (define-format blablah

    \ \ (:name "Blablah")

    \ \ (:suffix "bla"))

    \;

    (converter blablah-file latex-file

    \ \ (:require (url-exists-in-path? "bla2tex"))

    \ \ (:shell "bla2tex" from "\<gtr\>" to))
  </scheme-fragment>

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>