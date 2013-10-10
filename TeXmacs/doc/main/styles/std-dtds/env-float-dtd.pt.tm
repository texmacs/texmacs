<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambientes para objetos flutuantes>

  A <abbr|d.t.d.> <tmdtd|env-float> fornece marcação para objetos flutuantes.
  A etiqueta seguinte é a única de alto nível.

  <\explain|<markup|footnote>>
    Cria uma nota de rodpé.
  </explain>

  As etiquetas de baixo nível a seguir podem ser usadas para a definição de
  ambientes de alto nível para figuras e tabelas, como <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> e <markup|small-table>:

  <\explain|<markup|render-small-figure>>
    Um macro para exibição de uma figura pequena. Os argumento são um nome
    curto (como ``figura'' ou ``tabela'') para a lista de figuras, seu nome
    verdadeiro (como ```Figura 2.3'' ou ``Tabela <no-break>5''), a própria
    figura e um texto para a legenda.
  </explain>

  <\explain|<markup|render-big-figure>>
    Uma variação de <markup|render-small-figure> para exibição de figuras
    grandes.
  </explain>

  As etiquetas abaixo podem ser usadas para modificar a aparência do texto em
  torno das figuras, tabelas e notas de rodapé.

  <\explain|<markup|figure-name>>
    Um macro que controla a aparência do texto``Figura''. O padrão é usar
    negrito.
  </explain>

  <\explain|<markup|figure-sep>>
    O separador entre a figura e seu núme e a legenda. O padrão é um ponto
    seguido por um espaço.
  </explain>

  <\explain|<markup|footnote-sep>>
    O separador entre o número de uma nota de rodapé e o texto. O padrão é um
    ponto seguido por um espaço.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
  </collection>
</initial>