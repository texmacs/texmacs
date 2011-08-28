<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Compiling a bibliography>

  At the moment, <TeXmacs> uses <verbatim|bibtex> to compile bibliographies.
  The mechanism to automatically compile a bibliography is the following:

  <\itemize>
    <item>Write a <verbatim|.bib> file with all your bibliographic
    references. This file should have the format of a standard bibliography
    file for <LaTeX>.

    <item>Use <menu|Insert|Link|Citation> and <menu|Insert|Link|Invisible
    citation> to insert citations, which correspond to entries in your
    <verbatim|.bib> file.

    <item>At the place where your bibliography should be compiled, click on
    <menu|Insert|Automatic|Bibliography>. At the prompt, you should enter a
    <verbatim|bibtex> style (such as <verbatim|plain>, <verbatim|alpha>,
    <verbatim|abbrv>, etc.) and your <verbatim|.bib> file.

    <item>Use <menu|Document|Update|Bibliography> in order to compile your
    bibliography.
  </itemize>

  Notice that additional BiB<TeX> styles should be put in the directory
  <verbatim|~/.TeXmacs/system/bib>.

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