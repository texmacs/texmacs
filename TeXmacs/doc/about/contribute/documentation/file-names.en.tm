<TeXmacs|1.0.0.5>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conventions for the names of files>

  Most documentation should be organized as a function of the topic in a
  directory tree. The subdirectories of the top directory are the following:

  <\description>
    <expand|item*|devel>Documentation for developers.

    <expand|item*|examples>Examples of <TeXmacs> documents.

    <expand|item*|incoming>Incoming documentation, which is still a bit
    experimental.

    <expand|item*|main>The main documentation.

    <expand|item*|meta>How to write documentation and the compilation of
    documentation.
  </description>

  Please try to keep the number of entries per directory reasonably small.

  File names in the main directory should be of the form
  <verbatim|type-name.language.tm>. In the other directories, they are of the
  form <verbatim|name.language.tm>. Here <verbatim|type> is a major
  indication for the type of documentation; it should be one of the
  following:

  <\description>
    <expand|item*|adv>Documentation for advanced users.

    <expand|item*|man>For inclusion in the <TeXmacs> manual.

    <expand|item*|tut>For inclusion in the <TeXmacs> tutorial.
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
