<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion from <LaTeX> to <TeXmacs>>

  The current aim of the conversion program from <apply|LaTeX> to
  <apply|TeXmacs>, is to <with|font shape|italic|help> you in translating old
  documents into <apply|TeXmacs>. <with|font shape|italic|Grosso modo>,
  conversions from <apply|LaTeX> to <apply|TeXmacs> are more problematic than
  conversions the other way around. Nevertheless, as long as you restrict
  yourself to using the most common <apply|LaTeX> commands, you should be
  able to convert your old documents reasonably well. For example, all
  <apply|TeXmacs> help files have been written in <apply|LaTeX> in order to
  validate the <apply|LaTeX> to <apply|TeXmacs> conversion program.

  You may convert a <apply|LaTeX> document <verbatim|name.tex> into
  <apply|TeXmacs> using <subsubmenu|File|import|latex> and save it under
  <verbatim|name.tm>. If your <apply|LaTeX> document was written sufficiently
  well, then the converted result should be more or less acceptable, apart
  from certain unrecognized commands, which appear in red. A good solution
  would be to write your own style file for converted documents, based on the
  original style, and in which the unrecognized commands are defined.

  However, in certain less fortunate cases, the converted document will look
  like a great mess. This usually stems from the fact that <apply|TeX> and
  <apply|LaTeX> allow users to modify the parser dynamically, for instance
  using the <verbatim|\\catcode> command. In this case, the conversion
  program may get confused, by making erroneous assumptions on the mode or
  the environment. As a result, text may be converted as mathematics,
  mathematics as verbatim, and so on. Nevertheless, the commands in your
  source file <verbatim|name.tex> which confused the conversion program are
  usually easily localized by comparing the <apply|LaTeX> source with its
  <apply|TeXmacs> conversion. Modulo some hacking of the source, you should
  be able to remove the litigious code, so that the document converts
  reasonably well.

  In the future, we also plan to extend the conversion program with a style
  file converter and some additional features which facilitate the
  translation of user defined commands, which are defined in another document
  than the one you want to convert.

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
