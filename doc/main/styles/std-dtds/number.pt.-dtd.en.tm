<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Numbering environments>

  The <tmdtd|number-env> <abbr|d.t.d.> provides low-level tags for the
  numbering of standard environments. One of the most important tags is
  <markup|init-stdenv> which is used for resetting all environment counters.
  This is usually done at the start of each chapter or section, or once for
  the entire document.

  The <abbr|d.t.d.> also exports the very low-level tags
  <markup|newliststdenv>, <markup|newlistfigure> and
  <markup|newliststdenv-counter>, which control the numbering in
  collaboration with <tmdtd|env-manage>. The packages <tmpackage|number-us>
  and <tmpackage|number-europe> are provided for American-style and
  European-style numbering.

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
