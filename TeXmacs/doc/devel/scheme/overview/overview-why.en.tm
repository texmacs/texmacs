<TeXmacs|1.0.5.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Why <TeXmacs> uses <value|scheme> as its extension language>

  At a first glance, the choice of <value|scheme> as an extension language
  for <TeXmacs> may seem a bit strange for people who are accustomed to more
  conventional programming languages, such as <name|C++>, <name|Java>
  or<nbsp><name|Python>. In particular, its heavy use of parenthesis
  frightens more than one person.

  Our choice of <value|scheme> has been motivated by the fact that the
  language is highly flexible in several<nbsp>ways:

  <\enumerate>
    <item>It is easy to mix programs and data in a common framework.

    <item>It is easy to customize the language itself, by adding new
    programming constructs.

    <item>It is easy to write programs on a very abstract level.
  </enumerate>

  The first two features are very particular important for extension
  languages. Indeed, one major use of extension languages is to store data
  for the application (like keyboard shortcuts and menus) in an intelligent
  way. Furthermore, the application usually provides some very typical
  features, which may need to be reflected at the level of the extension
  language.

  For the first two features, the simplicity of the parenthesized notation
  used by <value|scheme> is also an advantage. Indeed, consider the
  following fragment of the definition of the <menu|File> menu:

  <\scheme-fragment>
    (menu-bind file-menu

    \ \ ("New" (new-buffer))

    \ \ ("Load" (choose-file load-buffer "Load file" ""))

    \ \ ("Save" (save-buffer))

    \ \ ...)
  </scheme-fragment>

  The entries of the menu (the data) and the corresponding actions (the
  programs) are very readable using the bracket notation. Similarly, when
  defining a new language primitive, the systematic use of the bracket
  notation relieves the user from the burden of making the corresponding
  changes in the parser.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>