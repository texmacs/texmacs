<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversione di documenti <TeXmacs> in Html>

  Abbiamo cominciato ad implementare la conversione tra i formati HTML e
  <apply|TeXmacs>. Per il momento, è possibile solamente importare documenti
  HTML utilizzando <apply|menu|File|Import|Html>. La maggior parte di HTML
  2.0 e alcune parti di HTML 3.0 sono attualmente supportate. Comunque, non è
  ancora possibile la navigazione. Nel futuro, implementeremo Math-ML.

  Quando si importano documenti HTML, i file i cui nomi cominciano con
  <verbatim|http:> o <verbatim|ftp:> saranno scaricati dalla rete usando
  <verbatim|wget>. Se ci si compila <apply|TeXmacs> personalmente, si può
  scaricare <verbatim|wget> da

  <\verbatim>
    \ \ ftp://ftp.gnu.org/pub/gnu/wget/
  </verbatim>

  Nelle distribuzioni binarie, <verbatim|wget> è già incluso.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia
  Gecchelin|Andrea Centomo>

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
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Importa>|<with|font
      family|<quote|ss>|Html>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
