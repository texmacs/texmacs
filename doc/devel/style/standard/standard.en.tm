<TeXmacs|1.0.0.5>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|The standard <TeXmacs> styles and packages>

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

  We notice that the theorem like environments are not standard in
  <apply|LaTeX>, which is a standard source of non compatibility. New
  ``theorems'' can be added with the <verbatim|newtheorem> command. It is
  also possible to add new ``remarks'' with the <verbatim|newremark> command;
  ``remarks'' are different from ``theorems'' in the sense that their body is
  usually not typeset in an emphasized font.

  Of course, programming environments are not supported by <apply|LaTeX>
  either. Such environments are currently under development.

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

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|edit tree>|<pageref|idx-1>>

      <tuple|<tuple|operators>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
