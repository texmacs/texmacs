<TeXmacs|1.99.21>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Books and multifile documents>

  When a document gets really large, you may want to subdivide it into
  smaller pieces. This both makes the individual pieces more easily reusable
  in other works and it improves the editor's responsiveness. An entire file
  can be inserted into another one using <menu|Insert|Link|Include>. In order
  to speed up the treatment of included documents, they are being buffered.
  In order to update all included documents, you should use
  <menu|Tools|Update|Inclusions>.

  When writing a book, one usually puts the individual chapters in files
  <verbatim|c1.tm>, <verbatim|c2.tm> until <verbatim|cn.tm>. One next creates
  one file <verbatim|book.tm> for the whole book, in which the files
  <verbatim|c1.tm>, <verbatim|c2.tm> until <verbatim|cn.tm> are included
  using the above mechanism. The table of contents, bibliography, etc. are
  usually put into <verbatim|book.tm>.

  In order to see cross references to other chapters when editing a
  particular chapter <verbatim|ci.tm>, one may specify <verbatim|book.tm> as
  a \Pmaster file\Q for the files <verbatim|c1.tm> to <verbatim|cn.tm> using
  <menu|Tools|Project|Attach master>. Currently, the chapter numbers
  themselves are not dealt with by this mechanism, so you may want to
  manually assign the environment variable <src-var|chapter-nr> at the start
  of each chapter file in order to get the numbering right when editing.

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>