<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Gerando um índice>

  Para criar um índice, você precisa inicialmente adicionar entradas no seu
  documento usando o menu <apply|menu|Insert|Link|Index entry>.
  Posteriormente, você deve colocar o cursor no local onde você deseja que o
  índice seja criado e clicar em <apply|menu|Insert|Automatic|Index>. O índice
  será gerado de forma análoga ao sumário.

  No menu <apply|menu|Insert|Link|Index entry> você encontra vários tipos de
  entradas para índice. As mais simples são ``main'', ``sub'', ``subsub'',
  que são macros com um, dois e três argumentos, respectivamente. Entradas
  das formas ``sub'', ``subsub'', podem ser usadas para subordinar as
  entradas do índice com respeito a outras entradas.

  Uma entrada de índice completa necessita de quatro argumentos. O primeiro é
  a chave com a qual a entrada será ordenada e ele deve ser uma ``tupla''
  (criada com <key|inactive \<less\>>) na qual o primeiro componente é a
  categoria principal, o segundo uma subcategoria, etc. O segundo argumento
  de uma entrada complexa pode ser vazio ou então ``strong'', indicando que
  esta entrada aparecerá em negrito no índice. O terceiro argumento
  normalmente é vazio, porém se você criar duas entradas com o mesmo terceiro
  argumento não vazio, então isto criará uma ``faixa'' de números de páginas.
  O quarto argumento, que é novamente uma ``tupla'', é a própria entrada.

  Também é possível criar uma linha no índice sem um número de página
  correspondente, usando ``interjeição'' em <apply|menu|Insert|Link|Index
  entry>. O primeiro argumento deste macro é a chave para ordenação da linha
  no índice e o segundo argumento contém o texto em si. Esta técnica pode ser
  útil para criar diferentes seções ``A'', ``B'', etc. no seu documento.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Item do
      índice>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Automático>|<with|font
      family|<quote|ss>|Índice>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Item do
      índice>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Item do
      índice>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
