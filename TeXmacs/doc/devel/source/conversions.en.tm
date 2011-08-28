<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Converters to other data formats>

  Currently, we have implemented imperfect converters between <apply|TeXmacs>
  and <apply|LaTeX> and from html to <apply|TeXmacs>. We hope that someone
  else will be willing to write better converters from scratch. This chapter
  has been included in order to give some recommendations in that direction
  based on our experience from the implementation of the actual conversion
  programs. We also recommend to take a look at the current implementations
  in the directory <verbatim|Convert>.

  <section|Parsing extern data formats>

  In order to write a converter from <apply|LaTeX> html, xml, etc. to
  <apply|TeXmacs>, a good first step is to write a parser for the extern data
  format. For html, xml, etc. this should be rather easy, but for
  <apply|LaTeX>, you will probably need to be a real <apply|LaTeX> guru
  (which I am not). We recommend the result of the parsing step to be a
  <apply|scheme> expression (something which is regrettable not the case for
  our actual converters), because this language is very well adapted for the
  implementation of the actual converter.

  This first step should be able to process any correct file having the
  extern data format; possible incompatibilities should only come into play
  during the actual conversion. In the case of <apply|LaTeX>, one should not
  expand the macros and keep all macro definitions, because <apply|TeXmacs>
  will be able to take advantage out of this.

  <section|The actual converter>

  We recommend the actual converter to proceed in several steps. Often it is
  convenient to start with a rough, structural, conversion step, which is
  ``polished'' by a certain number of additional steps. These additional
  steps may take care of some very particular layout issues which can not be
  treated conveniently at the main step.

  Actually, the main difficulties usually come from exceptional text, like
  verbatim, and layout issues which are handled differently in the extern
  data format and <apply|TeXmacs>. A good example of such a difference
  between <apply|LaTeX> and <apply|TeXmacs> is the way equations or lists are
  handled. Consider for instance the following paragraph:

  Text before.

  <\expand|equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </expand>

  Text after.

  In <apply|LaTeX>, the equation is really seen as a part of the paragraph.
  Indeed, there will not be any blank line between ``Text before'' and the
  equation. However, for efficiency reasons, it is better to see the
  paragraph as three paragraphs in <apply|TeXmacs>, because the lines can be
  typesetted independently. Nevertheless, the equation environment will
  disable the indentation of ``Text after''.

  As a result of this anomaly, converted texts have to be postprocessed, so
  as to insert paragraph breaks at strategic places. It should be noticed
  that this step may be independent from the format which is actually being
  converted and that a similar reverse step may be implemented for backward
  conversions. We also notice that one needs an exhaustive list of all
  similar exceptional environments for this postprocessing step. Actually, a
  future version of <apply|TeXmacs> might come with an additional feature,
  which permits the automatic detection of such environments. This is also
  important from a semantical point of view, because one should be able to
  detect that the above example logically forms only one and not three
  paragraphs.

  <section|Backward conversions>

  Conversions from <apply|TeXmacs> to an extern data format are usually
  easier to implement, because the <apply|TeXmacs> data format is
  semantically rich. However, conversions to an extern data format without a
  <apply|TeX>-like macro facility give rise to the problem of macro expansion
  of non supported <apply|TeXmacs> functions or environments. We plan to
  write a facility for this, which you will be able to use when writing a
  converter from <apply|TeXmacs> to something else.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
  </collection>
</references>
