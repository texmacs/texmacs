<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Especificando o tamanho das células e tabelas>

  Usando os menus <apply|menu|Table|Cell width|Set width> e
  <apply|menu|Table|Cell height|Set height> você pode especificar a largura e
  a altura de uma célula. A largura ou altura especificadas podem ser
  consideradas de três maneiras distintas:

  <\description>
    <expand|item*|Modo mínimo.>A dimensão final da célula será o mínimo entre
    o valor especificado e a dimensão correspondente da caixa dentro da
    célula.

    <expand|item*|Modo exato.>A dimensão da célula será exatamente aquela
    especificada.

    <expand|item*|Modo máximo.>A dimensão final da célula será o máximo entre
    o valor especificado e a dimensão correspondente da caixa dentro da
    célula.
  </description>

  A largura da moldura e o enchimento da célula (explicados mais tarde) são
  consideradas no cálculo do tamanho da caixa dentro da célula.

  Você pode também especificar a largura e altura para toda a tabela em
  <apply|menu|Table|Special table properties>. Em particular, você pode
  especificar a largura ou altura da tabela e você escolher como o espaço em
  branco é distribuído pelas células usando \ <apply|menu|Table|Special cell
  properties|Distribute unused space>. A convenção padrão é que o espaço é
  distribuído igualmente.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Ramiro Brito
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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Largura da célula>|<with|font
      family|<quote|ss>|Definir largura>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Altura da célula>|<with|font
      family|<quote|ss>|Definir altura>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Propriedades especiais de tabela>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabela>|<with|font
      family|<quote|ss>|Propriedades especiais de célula>|<with|font
      family|<quote|ss>|Distribuir espaço vazio>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
