<TeXmacs|1.0.0.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Books and multifile documents>

  When a document gets really large, you may want to subdivide it into
  smaller pieces. This both makes the individual pieces more easily reusable
  in other works and it improves the editor's responsiveness. An entire file
  can be inserted into another one using <apply|menu|Insert|Link|Include>. In
  order to speed up the treatment of included documents, they are being
  buffered. In order to update all included documents, you should use
  <apply|menu|Tools|Update|Inclusions>.

  When writing a book, one usually puts the individual chapters in files
  <verbatim|c1.tm>, <verbatim|c2.tm> until <verbatim|cn.tm>. One next creates
  one file <verbatim|book.tm> for the whole book, in which the files
  <verbatim|c1.tm>, <verbatim|c2.tm> until <verbatim|cn.tm> are included
  using the above mechanism. The table of contents, bibliography, etc. are
  usually put into <verbatim|book.tm>.

  In order to see cross references to other chapters when editing a
  particular chapter <verbatim|ci.tm>, one may specify <verbatim|book.tm> as
  a ``master file'' for the files <verbatim|c1.tm> to <verbatim|cn.tm> using
  <apply|menu|Document|Master|Attach>. Currently, the chapter numbers
  themselves are not dealt with by this mechanism. You may want to manually
  assign the environment variable <verbatim|chapternr> at the start of each
  chapter file in order to get the numbering right when editing.

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
