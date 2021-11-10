<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Bordas, enchimento e cores de fundo>

  Você pode especificar a espessura das bordas e o espaço vazio de enchimento
  dentro da célula em todas as quatro direções possíveis: à esquerda,
  direita, acima e abaixo (ver <apply|menu|Table|Cell border>). Você tem
  atalhos de teclado da forma \ <key|table b><render-key|<with|mode|math|x>> e
  <key|table p><render-key|<with|mode|math|x>> para especificar a espessura da
  moldura e do enchimento.

  A largura padrão das molduras para células no ambiente bloco é
  <verbatim|1ln>, isto é, a largura normal da linha na fonte corrente (como a
  largura de um traço de fração). Esta largura é usada à direita e abaixo de
  cada célula (exceto quando a célula está na primeira ou última célula). O
  enchimento horizontal padrão é <verbatim|1spc>: a largura de um espaço em
  branco na fonte corrente. O enchimento vertical padrão é <verbatim|1sep>: a
  separação padrão mínima entre duas caixas adjacentes.

  Cores podem ser atribuídas ao fundo das células com o menu
  <apply|menu|Table|Cell background color>.

  Também é possível atribuir uma moldura e um enchimento padrão para toda a
  tabela com \ o menu <apply|menu|Table|Special table properties|Border>.
  Neste caso, o espaço correspondente ao enchimento é aplicado do lado de
  fora da moldura.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

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
    <associate|language|portuguese>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Borda da célula>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Cor de fundo da célula>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Propriedades especiais de tabela>|<with|font
      family|<quote|ss>|Borda>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
