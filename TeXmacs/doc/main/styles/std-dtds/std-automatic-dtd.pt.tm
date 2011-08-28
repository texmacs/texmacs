<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Geração de conteúdo automática>

  A <abbr|d.t.d.> <tmdtd|std-automatic> contém definições para a geração de
  conteúdo automático como índices e bibliografia assim como para a
  apresentação deste material. As etiquetas abaixo são usadas para
  bibliografias:

  <\description>
    <expand|item*|<markup|cite>>Uma função com um número arbitrário de
    argumentos. Cada argumento é uma citação correspondente a um item em um
    arquivo BiB-<TeX>. As citações são mostradas da mesma forma em que são
    referenciadas na bibliografia e elas também fornecem hiperligações para
    as referências correspondentes. As citações são mostradas como pontos de
    interrogação se você não gerou a bibliografia.

    <expand|item*|<markup|nocite*>>Semelhante a <markup|cite>, mas as
    citações não são exibidas no texto principal.

    <expand|item*|<markup|bibitem*>>Uma função que especifica como exibir um
    texto na bibliografia.
  </description>

  As etiquetas abaixo são usadas para compilar um índice:

  <\description>
    <expand|item*|<markup|toc-main-1>>Uma função com um argumento para criar
    uma entrada inicial no índice. Esta função pode por exemplo ser usada
    quando um livro é composto de várias partes.

    <expand|item*|<markup|toc-main-2>>Uma função com um argumento para criar
    uma entrada principal no índice. Esta função normalmente é usada para
    capítulos.

    <expand|item*|<markup|toc-normal-1>>Uma função com um argumento para
    criar uma entrada normal no índice. Esta função normalmente é usada para
    sessões.

    <expand|item*|<markup|toc-normal-2>>Semelhante a <markup|toc-normal-2>,
    para entradas menos importantes como subsessões.

    <expand|item*|<markup|toc-normal-3>>Semelhante a <markup|toc-normal-3>,
    para entradas ainda menos importantes.

    <expand|item*|<markup|toc-small-1>>Usada para entradas pouco importantes,
    tais como parágrafos (podem ser ignoradas.)

    <expand|item*|<markup|toc-small-2>>Usada para entradas ainda menos
    importantes do que <markup|toc-small-1>, como subparágrafos.

    <expand|item*|<markup|toc-dots>>A separação entre uma entrada do índice e
    o número da página correspondente. Normalmente, são usados pontos
    horizontais.
  </description>

  As seguintes etiquetas são usadas para índices remissívos (no final do
  documento):

  <\description>
    <expand|item*|<markup|index>>Uma função com um argumento <var|x>, que
    insere <var|x> no índice como uma entrada principal.

    <expand|item*|<markup|subindex>>Uma função com dois argumentos, <var|x> e
    <var|y>, que insere <var|y> no índice como uma entrada subordinada a
    <var|x>.

    <expand|item*|<markup|subsubindex>>Uma função com três argumentos,
    <var|x>, <var|y> e <var|z>, que insere <var|z> no índice como uma entrada
    subordinada a <var|y>, que é subordinada a <var|x>.

    <expand|item*|<markup|index-complex>>Uma função com quatro argumentos,
    <var|key>, <var|how>, <var|range>, <var|entry>, que é documentada na
    sessão sobre <apply|hyper-link|geração de índices|
    ../../links/man-index.pt.tm>.

    <expand|item*|<markup|index-line>>Esta função aceita uma <var|chave> como
    argumento, que diz como ordenar a entrada, e a <var|entry> em si. Não é
    gerado um número de página.

    <expand|item*|<markup|index-1>>Macro com uma entrada no índice e um
    número de página, que é usado para exibir uma entrada principal do
    índice.

    <expand|item*|<markup|index-1*>>Semelhante a <markup|index-1>, mas sem o
    número da página.

    <expand|item*|<markup|index-<with|mode|math|n>>>(com <with|mode|math|n>
    entre <with|mode|math|1> e <with|mode|math|5>): macro com uma entrada no
    índice e um número de página, que é usada para apresentação de uma
    entrada do índice de nível <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Semelhante a
    <markup|index-<with|mode|math|n>>, mas sem o número da página.

    <expand|item*|<markup|index-dots>>O macro que produz os pontos entre as
    entradas do índice e os números de página correspondentes.
  </description>

  As etiquetas abaixo são usadas para glossários:

  <\description>
    <expand|item*|<markup|glossary>>Uma função que insere seu argumento único
    no glossário.

    <expand|item*|<markup|glossary-dup>>Para criar um número de página
    adicional para uma entrada que já tinha sido inserida no glossário.

    <expand|item*|<markup|glossary-explain>>Uma função para inserir uma
    entrada do glossário com sua explicação.

    <expand|item*|<markup|glossary-line>>Insere um entrada no glossário sem
    um número de página.

    <expand|item*|<markup|glossary-1>>Macro para apresentação de uma entrada
    do glossário com o seu número de página correspondente.

    <expand|item*|<markup|glossary-2>>Macro para apresentação de uma entrada
    do glossário, sua explicação, e seu número de página.

    <expand|item*|<markup|glossary-dots>>O macro que produz os pontos entre a
    entrada no glossário e seu número de página correspondente.
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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-automatic>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nocite*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|bibitem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-1>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-2>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-1>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-2>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-dots>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-35>>
    </associate>
  </collection>
</auxiliary>
