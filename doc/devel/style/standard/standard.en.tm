<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|The standard <TeXmacs> styles and packages>

  Before writing your own style file, it may be useful to take a look at some
  standard style files. For instance, you may load <verbatim|book.ts> using
  <menu|File|Load> (no path is necessary here, since the style directory is
  included in the default file path).

  After loading <verbatim|book.ts>, you will see many function and
  environment declarations (these declarations are visible, since style files
  are written in ``preamble mode'' (see <menu|Options|Mode>)). Some more
  declarations are contained in the files <verbatim|basic.ts>,
  <verbatim|list.ts>, <verbatim|theorem.ts> and <verbatim|program.ts> on
  which <verbatim|book.ts> is based. These files respectively contain basic,
  itemize-like, theorem-like and programming environments.

  Currently, the following standard document styles have been implemented:

  <\itemize>
    <item>Book;

    <item>Article;

    <item>Letter;

    <item>Seminar (for transparencies).
  </itemize>

  Each of these styles export a certain number of standard functions and
  environments listed below. All future standard document styles are expected
  to support at least the above commands and environments and we suggest
  users to write style files which do so too.

  <\itemize>
    <item>Sectioning commands.

    <item>Itemize and enumerate environments.

    <item>Equation like environments.

    <item>Theorem like environments.

    <item>Programming environments.
  </itemize>

  We notice that the theorem like environments are not standard in <LaTeX>,
  which is a standard source of non compatibility. New ``theorems'' can be
  added with the <verbatim|newtheorem> command. It is also possible to add
  new ``remarks'' with the <verbatim|newremark> command; ``remarks'' are
  different from ``theorems'' in the sense that their body is usually not
  typeset in an emphasized font.

  Of course, programming environments are not supported by <LaTeX> either.
  Such environments are currently under development.

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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>