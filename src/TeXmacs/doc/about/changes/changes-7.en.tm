<TeXmacs|1.0.2.3>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macro expansion (1.0.2.3 -- 1.0.2.4)>

  An important internal change concerning the data format has been made:
  macro expansions like

  <\expand|scheme-fragment>
    (expand <em|tag> <em|arg-1> ... <em|arg-n>)
  </expand>

  are now replaced by hard-coded tags

  <\expand|scheme-fragment>
    (<em|tag> <em|arg-1> ... <em|arg-n>)
  </expand>

  This makes the internal representation match with the corresponding
  <name|Scheme> representation. However, many perverse errors might arise in
  the few upcoming versions. Please keep copies of your old files and report
  any suspicious behaviour to us.

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
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Formatting tags (1.0.2 --
      1.0.2.1)><value|toc-dots><pageref|toc-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>