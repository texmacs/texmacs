<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion from <TeXmacs> to <LaTeX>>

  The most common situation is that you want to convert an article from
  <apply|TeXmacs> to <apply|LaTeX>, in order to submit it to some journal.
  Given a <apply|TeXmacs> file <verbatim|name.tm>, you may convert it into a
  <apply|LaTeX> file <verbatim|name.tex> using
  <apply|menu|File|Export|Latex>. At a first stage, you may try to run
  <apply|LaTeX> on <verbatim|name.tex>, and see whether you obtain a
  satisfactory result. If so, then you should submit <verbatim|name.tex>
  together with the style file <verbatim|TeXmacs.sty>, which can be found in
  the directory <verbatim|$TEXMACS_PATH/misc/latex>.

  Often, the journal to which you submit uses its own style file, say
  <verbatim|journal.sty>. In that case, you should also copy the file\ 

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/styles/article.ts
  </verbatim>

  to\ 

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  and use <verbatim|journal> as your document style in
  <apply|menu|Document|Style|Other>. You may optionally edit
  <verbatim|journal.ts>, so that the article layout becomes closer to the
  journal's style. In some cases, you also have to create a new copy of
  <verbatim|TeXmacs.sty>, and modify some of the environments for
  compatibility with the journal's style file <verbatim|journal.sty>.

  If your first try to convert your document into <apply|LaTeX> did not yield
  a satisfactory result, then you will usually observe that only minor parts
  of the texts were not converted correctly. This may be due to three main
  reasons:

  <\itemize>
    <item>Your text uses some specific <apply|TeXmacs> features.

    <item>You used a <apply|TeXmacs> feature, which has not yet been
    implemented in the conversion algorithm.

    <item>You found a bug in the conversion algorithm.
  </itemize>

  These issues will be discussed in more detail in the next section.

  In case of problems, a naive strategy would be to correct the produced
  <apply|LaTeX> file and to send it to the journal. However, this strategy
  has the disadvantage that you have to make these corrections over and over
  again, each time that you convert your <apply|TeXmacs> file
  <verbatim|name.tm>, after having made some extra modifications. A better
  strategy is to use the <apply|menu|Insert|Specific|Latex> and
  <apply|menu|Insert|Specific|Texmacs> constructs to write text which is
  visible in the converted resp. original file only.

  For instance, assume that the word ``blauwbilgorgel'' is hyphenated
  correctly in the <apply|TeXmacs> source, but not in the <apply|LaTeX>
  conversion. Then you may proceed as follows:

  <\enumerate>
    <item>Select ``blauwbilgorgel''.

    <item>Click on <apply|menu|Insert|Specific|Texmacs> to make the text
    ``blauwbilgorgel'' <apply|TeXmacs>-specific.

    <item>Click on <apply|menu|Insert|Specific|Latex>.

    <item>Type the latex code <verbatim|blauw\\-bil\\-gor\\-gel> with the
    correct hyphenation.

    <item>Press <key|<key-return>> to activate the
    <apply|LaTeX>-specific text.
  </enumerate>

  In a similar fashion, you may insert <apply|LaTeX>-specific line breaks,
  page breaks, vertical space, style parameter modifications, etc.

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
    <associate|idx-5|<tuple|2.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Export>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Style>|<with|font
      family|<quote|ss>|Other>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Specific>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Specific>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Specific>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Specific>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
