<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Diagramação>

  Como uma regra geral, o próprio <apply|TeXmacs> é responsável pela
  diagramação do seu texto. Assim, mesmo sem querer proibir completamente que
  você o faça, nós não encorajamos que você tente fazer a diagramação do seu
  documento visualmente. Por exemplo, você não deve inserir espaços ou linhas
  em branco para criar espaçamento vertical ou horizontal; espaço adicional
  deve ser inserido explicitamente com <apply|menu|Format|Space>. Isto
  tornará seu texto mais robusto, no sentido de que você não terá que refazer
  a diagramação quando realizar pequenas alterações, que podem mudar quebras
  de linhas ou páginas, ou grandes alterações, como mudar o estilo do
  documento.

  Vários tipos comandos para inserção de espaçamento explícito foram
  implementados. Você pode inserir espaços rígidos com alturas ou larguras
  previamente especificadas. Espaços horizontais não tem altura, e são
  elásticos ou rígidos. O comprimento de um espaço elástico depende da forma
  com que o parágrafo está sendo hifenizado. Além disto, é possível inserir
  espaçamento que considera marcas de tabulação. Espaços verticais podem ser
  inseridos tanto no fim quanto no começo do parágrafo: o espaçamento
  adicional entre dois parágrafos é o máximo entre o espaçamento vertical
  antes do segundo parágrafo e o espaço após o primeiro parágrafo (em
  contraste com o <apply|TeX>, isto previne espaçamento desnecessário entre
  dois teoremas consecutivos).

  Quanto à formatação de um parágrafo, o usuário pode especificar o estilo do
  parágrafo (justificado, alinhado à direita, alinhado à esquerda), as
  margens do parágrafo e os espaçoes horizontais à esquerda e à direita no
  início e no final de cada parágrafo, respectivamente. O usuário também pode
  controlar o espaçamento entre parágrafos e entre as linhas sucessivas de
  cada parágrafo.

  Você pode especificar a formatação da página no menu
  <apply|menu|Document|Page>. Em primeiro lugar, você pode escolher a forma
  na qual as páginas são mostradas na tela do computador: escolhendo
  ``papel'' como o tipo da página em <apply|menu|Document|Page|Type>, você
  pode ver as quebras de página explicitamente. O padrão é formatar a página
  como ``papiro'', que evita a quebra de páginas durante a preparação do
  documento. O tipo ``automático'' admite que o tamanho do papel é exatamente
  o mesmo da janela do <apply|TeXmacs>. As margens da página e a largura do
  texto são especificados em <apply|menu|Document|Page|Layout>.
  Freqüentemente, é conveniente reduzir as margens para exibição na tela do
  computador; isto é feito em <apply|menu|Document|Page|Screen layout>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Espaço>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>|<with|font
      family|<quote|ss>|Tipo>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>|<with|font
      family|<quote|ss>|Layout>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Página>|<with|font family|<quote|ss>|Layout da
      tela>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
