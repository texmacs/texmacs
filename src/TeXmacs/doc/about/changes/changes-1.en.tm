<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Document format (0.3.4)>

  The TeXmacs document format has profoundly changed in order to make TeXmacs
  compatible with XML in the future. Most importantly, the old style
  environments like\ 

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>environment\|open\|close\<gtr\>\<gtr\
    \>,
  </verbatim>

  which are applied via matching pairs <verbatim|\<less\>begin\|env\<gtr\>tex\
  t\<less\>end\|env\<gtr\>>, have been replaced by macros\ 

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>macro\|body\|open\<less\>body\<gtr\>\
    close\<gtr\>\<gtr\>,
  </verbatim>

  which are applied via single macro expansions
  <verbatim|\<less\>expand\|env\|text\<gtr\>>. Similarly, matching pairs
  <verbatim|\<less\>set\|var\|val\<gtr\>text\<less\>reset\|var\<gtr\>> of
  environment variable changes are replaced by a
  <verbatim|\<less\>with\|var\|val\|text\<gtr\>> construct (close to XML
  attributes). From a technical point of view, these changes lead to several
  complications if the <verbatim|text> body consists of several paragraphs.
  As a consequence, badly structured documents may sometimes display
  differently in the new version (although I only noticed one minor change in
  my own documents). Furthermore, in order to maintain the higher level of
  structure in the document, the behaviour of the editor in relation to
  multiparagraph environments has slightly changed.

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
