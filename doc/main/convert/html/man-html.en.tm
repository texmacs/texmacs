<TeXmacs|1.0.0.5>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion of <TeXmacs> documents to Html>

  We have started to implemented the conversion between HTML and
  <apply|TeXmacs>. At this moment, it is only possible to import HTML
  documents using <subsubmenu|File|import|html>. Most of HTML 2.0 and parts
  of HTML 3.0 are currently supported. However, no browsing facilities have
  been added yet. In the future, we plan to implement Math-ML.

  When importing HTML documents, files whose names start with
  <verbatim|http:> or <verbatim|ftp:> will be downloaded from the web using
  <verbatim|wget>. If you compiled <apply|TeXmacs> yourself, then you can
  download <verbatim|wget> from\ 

  <\verbatim>
    \ \ ftp://ftp.gnu.org/pub/gnu/wget/
  </verbatim>

  In the binary distributions, we have included <verbatim|wget>.

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
