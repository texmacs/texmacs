<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Listas>

  Usando <apply|menu|Insert|Itemize> você pode criar um lista sem numeração.
  Você pode selecionar uma marca específica como <with|mode|math|\<bullet\>>
  (bolas), <with|mode|math|<op|->> (travessões) ou
  <with|mode|math|<op|\<rightarrow\>>> (setas) para indicar entradas na
  lista, ou usar a marca padrão. Listas pode ser<em| aninhadas> dentro de
  outras etiquetas, como na lista abaixo:

  <\itemize>
    <item>Primeiro item.

    <item>Aqui vem uma sublista:

    <\itemize>
      <item>Um sub-item.

      <item>Outro sub-item.
    </itemize>

    <item>Um ítem final.
  </itemize>

  A marca padrão é mostrada de forma diferente dependendo do nível de
  aninhamento. No nível mais externo, a marca é <with|mode|math|\<bullet\>>,
  no segundo nível é <with|mode|math|<op|\<circ\>>>, e assim por diante.
  Quando você está dentro de uma lista, note que pressionar
  \ <key|return> automaticamente começa um novo ítem. Se você
  necessita de ítens formados por vários parágrafos, então você pode usar
  <key|S-return> para iniciar um novo parágrafo.

  Ambientes de enumeração, que são criados usando
  <apply|menu|Insert|Enumerate>, comportam-se como aqueles criados acima,
  exceto que os ítens são numerados. Abaixo está um exemplo de enumeração que
  foi criado com <apply|menu|Insert|Enumerate|Roman>:

  <\expand|enumerate-Roman>
    <item>O primeiro item.

    <item>O segundo.

    <item>E um último.
  </expand>

  O último tipo de listas são listas descritivas. Elas são criadas com
  <apply|menu|Insert|Description> e permitem que você descreva uma lista de
  conceitos:

  <\description>
    <expand|item*|Gnu.>Um bicho cabeludo, mas manso.

    <expand|item*|Gnat.>Vive apenas nos zoológicos.
  </description>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|III.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Listar>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Enumerar>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Enumerar>|<with|font
      family|<quote|ss>|Roman>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Descrição>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
