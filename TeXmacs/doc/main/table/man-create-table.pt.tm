<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Criando Tabelas>

  Para criar uma tabela, você pode usar tanto <apply|menu|Insert|Table> ou um
  dos atalhos abaixo:

  <\description>
    <expand|item*|<key|table N t>>Criar uma tabela regular.

    <expand|item*|<key|table N T>>Criar uma tabela regular com células
    centradas.

    <expand|item*|<key|table N b>>Criar um ``bloco'', cujas células
    são separadas por linhas.

    <expand|item*|<key|table N B>>Cria um bloco cujas células são
    centradas.
  </description>

  No modo matemático, algumas outras estruturas similares a tabelas estão
  disponíveis:

  <\description>
    <expand|item*|<key|table N m>>Criar uma matriz.

    <expand|item*|<key|table N d>>Criar um determinante.

    <expand|item*|<key|table N c>>Criar uma lista de opções.
  </description>

  O ambiente <verbatim|\\eqnarray*> é também um tipo especial de estrutura
  semelhante a uma tabela, que se estende por toda uma linha. Você pode
  iniciar uma lista de equações usando <apply|menu|Insert|Mathematics|Equations>.

  Em uma tabela recém criada, seu tamanho é o mínimo (em geral
  <with|mode|math|1\<times\>1>) e suas células são vazias. Novas linhas e
  colunas podem ser inseridas com as combinações <shortcut|(structured-insert-left)>,
  <shortcut|(structured-insert-right)>, <shortcut|(structured-insert-up)> e
  <shortcut|(structured-insert-down)>. Por exemplo, <shortcut|(structured-insert-right)> \ cria
  uma nova coluna à direita da posição corrente do cursor. Você também pode
  começar uma nova linha após a posição corrente do cursor digitando
  <key|return>.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Tabela>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Matemáticos>|<with|font
      family|<quote|ss>|Equações>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
