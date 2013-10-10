<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Geração de conteúdo automática>

  A <abbr|d.t.d.> <tmdtd|std-automatic> contém definições para a geração de
  conteúdo automático como índices e bibliografia assim como para a
  apresentação deste material. As etiquetas abaixo são usadas para
  bibliografias:

  <\explain|<markup|cite>>
    Uma função com um número arbitrário de argumentos. Cada argumento é uma
    citação correspondente a um item em um arquivo BiB-<TeX>. As citações são
    mostradas da mesma forma em que são referenciadas na bibliografia e elas
    também fornecem hiperligações para as referências correspondentes. As
    citações são mostradas como pontos de interrogação se você não gerou a
    bibliografia.
  </explain>

  <\explain|<markup|nocite*>>
    Semelhante a <markup|cite>, mas as citações não são exibidas no texto
    principal.
  </explain>

  <\explain|<markup|bibitem*>>
    Uma função que especifica como exibir um texto na bibliografia.
  </explain>

  As etiquetas abaixo são usadas para compilar um índice:

  <\explain|<markup|toc-main-1>>
    Uma função com um argumento para criar uma entrada inicial no índice.
    Esta função pode por exemplo ser usada quando um livro é composto de
    várias partes.
  </explain>

  <\explain|<markup|toc-main-2>>
    Uma função com um argumento para criar uma entrada principal no índice.
    Esta função normalmente é usada para capítulos.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Uma função com um argumento para criar uma entrada normal no índice. Esta
    função normalmente é usada para sessões.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Semelhante a <markup|toc-normal-2>, para entradas menos importantes como
    subsessões.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Semelhante a <markup|toc-normal-3>, para entradas ainda menos
    importantes.
  </explain>

  <\explain|<markup|toc-small-1>>
    Usada para entradas pouco importantes, tais como parágrafos (podem ser
    ignoradas.)
  </explain>

  <\explain|<markup|toc-small-2>>
    Usada para entradas ainda menos importantes do que <markup|toc-small-1>,
    como subparágrafos.
  </explain>

  <\explain|<markup|toc-dots>>
    A separação entre uma entrada do índice e o número da página
    correspondente. Normalmente, são usados pontos horizontais.
  </explain>

  As seguintes etiquetas são usadas para índices remissívos (no final do
  documento):

  <\explain|<markup|index>>
    Uma função com um argumento <var|x>, que insere <var|x> no índice como
    uma entrada principal.
  </explain>

  <\explain|<markup|subindex>>
    Uma função com dois argumentos, <var|x> e <var|y>, que insere <var|y> no
    índice como uma entrada subordinada a <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Uma função com três argumentos, <var|x>, <var|y> e <var|z>, que insere
    <var|z> no índice como uma entrada subordinada a <var|y>, que é
    subordinada a <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Uma função com quatro argumentos, <var|key>, <var|how>, <var|range>,
    <var|entry>, que é documentada na sessão sobre <hlink|geração de índices|
    ../../links/man-index.pt.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Esta função aceita uma <var|chave> como argumento, que diz como ordenar a
    entrada, e a <var|entry> em si. Não é gerado um número de página.
  </explain>

  <\explain|<markup|index-1>>
    Macro com uma entrada no índice e um número de página, que é usado para
    exibir uma entrada principal do índice.
  </explain>

  <\explain|<markup|index-1*>>
    Semelhante a <markup|index-1>, mas sem o número da página.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (com <math|n> entre <math|1> e <math|5>): macro com uma entrada no índice
    e um número de página, que é usada para apresentação de uma entrada do
    índice de nível <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Semelhante a <markup|index-<math|n>>, mas sem o número da página.
  </explain>

  <\explain|<markup|index-dots>>
    O macro que produz os pontos entre as entradas do índice e os números de
    página correspondentes.
  </explain>

  As etiquetas abaixo são usadas para glossários:

  <\explain|<markup|glossary>>
    Uma função que insere seu argumento único no glossário.
  </explain>

  <\explain|<markup|glossary-dup>>
    Para criar um número de página adicional para uma entrada que já tinha
    sido inserida no glossário.
  </explain>

  <\explain|<markup|glossary-explain>>
    Uma função para inserir uma entrada do glossário com sua explicação.
  </explain>

  <\explain|<markup|glossary-line>>
    Insere um entrada no glossário sem um número de página.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro para apresentação de uma entrada do glossário com o seu número de
    página correspondente.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro para apresentação de uma entrada do glossário, sua explicação, e
    seu número de página.
  </explain>

  <\explain|<markup|glossary-dots>>
    O macro que produz os pontos entre a entrada no glossário e seu número de
    página correspondente.
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