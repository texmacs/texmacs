<TeXmacs|1.0.1.12>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Compiling a bibliography>

  At the moment, <apply|TeXmacs> uses <verbatim|bibtex> to compile
  bibliographies. The mechanism to automatically compile a bibliography is
  the following:

  <\itemize>
    <item>Write a <verbatim|.bib> file with all your bibliographic
    references. This file should have the format of a standard bibliography
    file for <apply|LaTeX>.

    <item>Use <apply|menu|Insert|Link|Citation> and
    <apply|menu|Insert|Link|Invisible citation> to insert citations, which
    correspond to entries in your <verbatim|.bib> file.

    <item>At the place where your bibliography should be compiled, click on
    <apply|menu|Text|Automatic|Bibliography>. At the prompt, you should enter
    a <verbatim|bibtex> style (such as <verbatim|plain>, <verbatim|alpha>,
    <verbatim|abbrv>, etc.) and your <verbatim|.bib> file.

    <item>Use <apply|menu|Document|Update|Bibliography> in order to compile
    your bibliography.
  </itemize>

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Citation>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Invisible
      citation>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Automatic>|<with|font
      family|<quote|ss>|Bibliography>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Update>|<with|font
      family|<quote|ss>|Bibliography>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
