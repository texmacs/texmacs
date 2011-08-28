<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Inserindo imagens>

  Você pode incluir imagens no texto usando o menu <apply|menu|Insert|Image>.
  O <apply|TeXmacs> reconhece atualmente os formatos de arquivo
  <verbatim|ps>, <verbatim|eps>, <verbatim|tif>, <verbatim|pdf>,
  <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>, <verbatim|xpm> and
  <verbatim|fig>. O programa <verbatim|gs> ( ghostscript) é usado para exibir
  imagens postscript. Se o ghostscript não estiver instalado no seu sistema,
  você pode descarregá-lo de

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  Os outros tipos de arquivos são convertidos para postscript usando os
  scripts<verbatim| tiff2ps>, <verbatim|pdf2ps>, <verbatim|pnmtops>,
  <verbatim|giftopnm>, <verbatim|ppmtogif>, <verbatim|e xpmtoppm>. Se estes
  scripts não estiverem instalados no seu computador, por favor entre em
  contato com o seu administrador de sistemas.

  O padrão do <apply|TeXmacs> é mostrar as imagens no tamanho em que foram
  criadas. As operação abaixo funcionam sobre imagens:

  <\itemize>
    <item>Recortar as imagens com um retângulo de corte. O canto inferior
    esquerdo da imagem é usado como a origem para o retângulo de recorte.

    <item>Mudar o tamanho de uma imagem. Quando uma nova largura é
    especificada e altura é deixada livre (ou vice-versa), o tamanho da
    imagem muda de forma a preservar a razão de aspecto original.

    <item>Aumentar ou reduzir a imagem. Uma maneira alternativa que
    multiplica a altura e a largura da imagem pela mesma constante
  </itemize>

  Também incluímos um script para converter figuras, opcionalmente com
  fórmulas do <apply|LaTeX>, em postscript encapsulado. Para incluir uma
  fórmula do <apply|LaTeX> em uma figura do <verbatim|xfig>, lembramos que
  você deveria entrar com a fórmula como texto, selecionando uma fonte do
  <apply|LaTeX> e ativando a opção ``special'' nas opções de texto.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Imagem>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
