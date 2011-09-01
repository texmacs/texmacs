<TeXmacs|1.0.7.12>

<style|tmdoc>

<\body>
  <tmdoc-title|Conventions for the names of files>

  Most documentation should be organized as a function of the topic in a
  directory tree. The subdirectories of the top directory are the following:

  <\description>
    <item*|about>Various information about the <TeXmacs> system (authors,
    changes, <abbr|etc.>).

    <item*|devel>Documentation for developers.

    <item*|main>The main documentation.
  </description>

  Please try to keep the number of entries per directory reasonably small.

  File names in the main directory should be of the form
  <verbatim|type-name.language.tm>. In the other directories, they are of the
  form <verbatim|name.language.tm>. Here <verbatim|type> is a major
  indication for the type of documentation; it should be one of the
  following:

  <\description>
    <item*|man>For inclusion in the <TeXmacs> manual.

    <item*|tut>For inclusion in the <TeXmacs> tutorial.
  </description>

  You should try to keep the documentation on the same topic together,
  regardless of the type. Indeed, this allows you to find more easily all
  existing documentation on a particular topic. Also, it may happen that you
  want to include some documentation which was initially meant for the
  tutorial in the manual. The <verbatim|language> in which is the
  documentation has been written should be a two letter code like
  <verbatim|en>, <verbatim|fr>, etc. The main <verbatim|name> of your file
  should be the same for the translations in other languages. For instance,
  <verbatim|man-keyboard.en.tm> should not be translated as
  <verbatim|man-clavier.fr.tm>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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