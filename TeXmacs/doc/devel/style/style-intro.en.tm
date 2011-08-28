<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|<TeXmacs> style files>

  One of the fundamental strengths of <TeXmacs> is the possibility to write
  your own style files and packages. The purpose of style files is multiple:

  <\itemize>
    <item>They allow the abstraction of repetitive elements in texts, like
    sections, theorems, enumerations, etc.

    <item>They form a mechanism which allow you to structure your text. For
    instance, you may indicate that a given portion of your text is an
    abbreviation, a quotation or ``important''.

    <item>Standard document styles enable you to write professionally looking
    documents, because the corresponding style files have been written with a
    lot of care by people who know a lot about typography and aesthetics.
  </itemize>

  To a document, it is possible to associate one or several document styles,
  which are either standard or user defined. The main document style of a
  document is selected in the <apply|menu|Document|Style> menu. Extra styles
  can be added using <apply|menu|Document|Use package>.

  From the editor point of view, each style corresponds to a <verbatim|.ts>
  file. The files corresponding to each style are processed in as if they
  were usual documents, but at the end, the editor only keeps the final
  environment as the initial environment for the main document. More
  precisely, the style files are processed in order as well as there own
  styles, in a recursive manner.

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
