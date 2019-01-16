<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Standard utilities>

  Besides the basic concepts from the previous sections, which underly the
  scheme API for <TeXmacs>, the <scheme> kernel implements several other
  utilities and language extensions. In this section, we will briefly sketch
  some of them on hand of examples. Further details can be found in the
  chapter about <hlink|<TeXmacs> extensions to <scheme> and
  utilities|../utils/scheme-utils.en.tm>.

  <paragraph*|Regular expressions>

  <TeXmacs> implements the routines <scm|match?> and <scm|select> for
  matching regular expressions and selecting subexpressions along a \Ppath\Q.
  These routines both work for the <verbatim|content> type. For instance, in
  order to search all expressions of the form

  <\equation*>
    <frac|<with|color|brown|a>|1+<sqrt|<with|color|brown|b>>>
  </equation*>

  in the current buffer, where <math|<with|color|brown|a>> and
  <math|<with|color|brown|b>> are general expressions, one may use the
  following <scheme> command:

  <\session|scheme|default>
    <\input|scheme] >
      (select (buffer-tree) '(:* (:match (frac :%1 (concat "1+" (sqrt
      :%1))))))
    </input>
  </session>

  <paragraph*|Dialogues>

  <TeXmacs> supports several commands for interactive dialogues with the
  user. For instance, when executing the following scheme command, you will
  be prompted for two numbers, whose product will be displayed in the footer:

  <\session|scheme|default>
    <\input|Scheme] >
      (user-ask "First number:"

      \ \ (lambda (a)

      \ \ \ \ (user-ask "Second number:"

      \ \ \ \ \ \ (lambda (b)

      \ \ \ \ \ \ \ \ (set-message (number-\<gtr\>string (*
      (string-\<gtr\>number a)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-\<gtr\>number
      b)))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "product")))))
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <paragraph*|User preferences>

  When writing a plug-in, you may wish to define some new user preferences.
  This can be done using the <scm|define-preferences> command, which adds a
  list of user preferences, together with their default values and a
  call-back routine. The call-back routine is called whenever you change the
  corresponding preference. For instance:

  <\scm-code>
    (define-preferences

    \ \ ("Gnu's hair color" "brown" notify-gnu-hair-change)

    \ \ ("Snail's cruising speed" "1mm/sec" notify-Achilles))
  </scm-code>

  Preferences can be set, reset and read using <scm|set-preference>,
  <scm|reset-preference> and <scm|get-preference>.

  <paragraph*|New data formats and converters>

  New data formats and converters can be declared using the
  <scm|define-format> and <scm|converter> instructions. When a format can be
  converted from or into <TeXmacs>, then it will automatically appear into
  the <menu|File|Export> and <menu|File|Import> menus. Similarly, when a
  format can be converted to <name|Postscript>, then it also becomes a valid
  format for images. <TeXmacs> also attempts to combine explicitly declared
  converters into new ones.

  Typically, the declaration of a new format and a converter would look like:

  <\scm-code>
    (define-format blablah

    \ \ (:name "Blablah")

    \ \ (:suffix "bla"))

    \;

    (converter blablah-file latex-file

    \ \ (:require (url-exists-in-path? "bla2tex"))

    \ \ (:shell "bla2tex" from "\<gtr\>" to))
  </scm-code>

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>