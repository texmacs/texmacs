<TeXmacs|1.0.0.3>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Planned changes in the <TeXmacs> document format>

  <\enumerate>
    <item>Currently, the arities of <apply|TeXmacs> operators are not allowed
    to vanish (in which case they become leaves). For operators
    like<verbatim| concat> this is not very clean nor very handy. We
    therefore plan to allow arities of zero. The formatting operators of the
    form <verbatim|(format how)> will in particular become operators of arity
    zero.

    <item>We are not happy at all with the way tables are formatted. We
    currently provided imperfect <verbatim|mosaic> and <verbatim|split>
    operators, which should be replaced with a single, more powerful
    operator. However, we are not yet very sure about the right choice.

    <item>Several extensions of the format should be foreseen. In
    <verbatim|Typeset/data/data.gen.cpp>, you will notice that we have already
    introduced the <verbatim|graphics>, <verbatim|point>, <verbatim|line>,
    <verbatim|arc> and <verbatim|bezier> operators, which are not yet
    implemented. Other future operators will concern footnotes, multicolums,
    dynamic texts, graphics, computer algebra output, database or spreadsheet
    facilities, etc.
  </enumerate>

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
