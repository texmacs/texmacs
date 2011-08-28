<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Propriedades avançadas>

  No conjunto de menus, você pode encontrar também muitas outras propriedades
  interessantes para tabelas. Resumidadmente, elas incluem o seguinte:

  <\itemize>
    <item>Fazer com que uma célula se sobreponha às células vizinhas acima e
    abaixo.

    <item>Criação de subtabelas inteiras dentro de uma célula.

    <item>Correção da profundidade e da altura do texto, para fazer com que
    as linhas de base coincidam.

    <item>Hifenização horizontal do conteúdo das células e hifenização
    vertical da tabela completa.

    <item>Colar várias linhas e/ou colunas juntas, de forma que as células
    coladas tornam-se ``parte da moldura'' das células remanescentes.

    <item>Desativação da tabela, para examinar e modificar seu ``código
    fonte''.

    <item>Ajustar o ``centro de extensão'' da tabela. Depois disto, as
    propriedades de formatação desta célula serão usadas para novas células
    criadas em torno deste centro.

    <item>Especificar o tamanho mínimo e máximo da tabela, que será
    respeitado quando a tabela for modificada (isto é útil principalmente
    para a criação de macros de tabelas).
  </itemize>

  Atualmente, todas as tabelas vem dentro de um ambiente como
  <markup|tabular>, <markup|block>, <markup|matrix>, etc. Quando você criar
  suas próprias tabelas, você pode usar <apply|menu|Table|Special table
  properties|Extract format> para extrair o formato de uma dada tabela.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven & Ramiro Brito
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
    <associate|language|english>
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
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|matrix>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Table>|<with|font
      family|<quote|ss>|Special table properties>|<with|font
      family|<quote|ss>|Extract format>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
