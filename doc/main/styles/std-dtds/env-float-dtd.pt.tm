<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambientes para objetos flutuantes>

  A <abbr|d.t.d.> <tmdtd|env-float> fornece marcação para objetos flutuantes.
  A etiqueta seguinte é a única de alto nível.

  <\description>
    <expand|item*|<markup|footnote>>Cria uma nota de rodpé.
  </description>

  As etiquetas de baixo nível a seguir podem ser usadas para a definição de
  ambientes de alto nível para figuras e tabelas, como <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> e <markup|small-table>:

  <\description>
    <expand|item*|<markup|small-figure*>>Um macro para exibição de uma figura
    pequena. Os argumento são um nome curto (como ``figura'' ou ``tabela'')
    para a lista de figuras, seu nome verdadeiro (como ```Figura 2.3'' ou
    ``Tabela <format|no line break>5''), a própria figura e um texto para a
    legenda.

    <expand|item*|<markup|big-figure*>>Uma variação de <markup|small-figure*>
    para exibição de figuras grandes.
  </description>

  As etiquetas abaixo podem ser usadas para modificar a aparência do texto em
  torno das figuras, tabelas e notas de rodapé.

  <\description>
    <expand|item*|<markup|figurename>>Um macro que controla a aparência do
    texto``Figura''. O padrão é usar negrito.

    <expand|item*|<markup|figuresep>>O separador entre a figura e seu núme e
    a legenda. O padrão é um ponto seguido por um espaço.

    <expand|item*|<markup|footnotesep>>O separador entre o número de uma nota
    de rodapé e o texto. O padrão é um ponto seguido por um espaço.
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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-float>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|footnote>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-figure>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-table>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-table>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure*>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|big-figure*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|small-figure*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|figurename>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|figuresep>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|footnotesep>>|<pageref|idx-12>>
    </associate>
  </collection>
</auxiliary>
