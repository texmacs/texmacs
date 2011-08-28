<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Escrevendo texto estruturado>

  Normalmente, documentos longos são estruturados: são organizados em
  capítulos, seções e subseções, contém diferentes tipos de textos, como
  texto normal, citações, notas de rodapé, teoremas, etc. Depois que você
  escolhe um <em|estilo de documento> em <apply|menu|Document|Style>, o
  próprio <apply|TeXmacs> cuida da diagramação, gerando automaticamente a
  numeração de seções, páginas e teoremas, bem como uma diagramação atraente
  para as notas de rodapé e citações e assim por diante.

  Quatro estilos de documentos padrão foram implementados até agora: carta,
  artigo, livro e seminário. O estilo seminário é usado para criação de
  transparências. Assim que você tenha escolhido um estilo, você pode
  organizar seu texto em seções (ver <apply|menu|Insert|Section>), e usar
  <em|ambientes> específicos. Exemplos de ambientes são teoremas,
  proposições, observações e similares (ver <apply|menu|Insert|Environment>).
  Outros exemplos são listas de ítens (ver <apply|menu|Insert|Itemize>) ou
  listas numeradas (ver <apply|menu|Insert|Enumerate>).

  Quando você se acostumar mais com o <apply|TeXmacs>, será possível
  adicionar seus próprios ambientes nos seu próprios arquivos de estilo.
  Suponha, por exemplo, que você freqüentemente faz citações e que você
  deseja que estas apareçam em itálico, com margens de 1cm à esquerda e à
  direita. Ao invés de mudar manualmente as propriedades do texto e do
  parágrafo todas as vezes em que você faz uma citação, é melhor criar um
  ambiente. Não só a criação de uma citação será mais rápida, mas também
  torna-se possível mudar sistematicamente a diagramação de suas citações ao
  longo de todo o documento, simplesmente pela mudança da definição deste
  ambiente. Esta situação pode ocorrer, por exemplo, se você descobre, <em|a
  posteriori,> que você prefere que as citações apareçam em uma fonte menor.

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
    <associate|toc-1|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Seção>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Ambiente>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Listar>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Enumerar>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
