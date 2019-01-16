<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Introduction>

  <TeXmacs> offers high quality converters to and from <LaTeX>. For simple
  documents, it suffices to use <menu|File|Export|LaTeX> <abbr|resp.>
  <menu|File|Import|LaTeX>. However, in order to take fully advantage out of
  the converts, it is necessary to understand some particularities of
  <LaTeX>.

  First of all, it should be emphasized that <TeX>/<LaTeX> is <em|not> a data
  format. Indeed, <TeX> is a programming language for which no real
  standardization process has taken place: valid <TeX> programs are defined
  as those which are recognized by the <TeX> program. In particular, there
  exists no formal specification of the language and it is not even clear
  what should be considered to be a valid <TeX> document. As a consequence of
  this, a converter from <LaTeX> to <TeXmacs> can only be designed to be 100%
  reliable for a (substantial) subset of the <TeX>/<LaTeX> language.

  A second important point is that publishers usually impose additional
  constraints on the kind of <LaTeX> documents which they accept for
  submissions. For instance, certain journals provide additional macros for
  title information, theorems, specific layout features, etc. Other journals
  forbid for the definition of new macros in the preamble. Since <TeXmacs> is
  <em|not> a <TeX>/<LaTeX> front-end, it is difficult for us to write
  specific code for each possible journal. Nevertheless, some general
  principles do hold, and we will describe below how to customize the
  converter so as to make the conversion process as simple and automatic as
  possible.

  Another point which should be stressed is that <TeXmacs> aims to provide a
  strict superset of <TeX>/<LaTeX>. This not completely the case yet, but it
  is already true that many features in <TeXmacs> admit no direct analogues
  in <TeX>/<LaTeX> or one of its packages. This is for instance the case for
  computer algebra sessions, folding, actions, graphics and presentations,
  but also for certain typesetting constructs, like vertical alignment and
  background filling in tables. When using such additional features, you
  should be prepared that they will not be converted correctly to <LaTeX>.

  Finally, when preparing journal papers with <TeXmacs>, <em|please> consider
  submitting them in <TeXmacs> format. The editors of the journal will
  probably force you to convert your paper to <LaTeX>, but repeated
  submissions in <TeXmacs> format will put pressure upon them to accept this
  new format.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>