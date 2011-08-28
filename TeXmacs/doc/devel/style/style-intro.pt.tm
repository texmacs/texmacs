<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Arquivos de estilo do <TeXmacs>>

  Uma das melhores características do <TeXmacs> é a possibilidade de escrever
  seus próprios arquivos de estilo. Arquivos de estilo tem multiplas funções:

  <\itemize>
    <item> Permitem a abstração de elementos repetitivos em textos como
    seções, teoremas, enumerações, etc.

    <item>Formam um mecanismo que permite que você estruture o seu texto. Por
    exemplo, você pode indicar que um trecho do seu texto é uma abreviação,
    uma citação ou ``importante''.

    <item>Alguns estilo padrão permitem que você escreva documentos com
    aparência profissional, porque foram projetados com muito cuidado por
    pessoas com muito conhecimento muito sobre tipografia e estética.
  </itemize>

  É possível associar um ou mais estilos a um documento. O estilo principal
  do documento é escolhido no menu <apply|menu|Document|Style>, e estilos
  adicionais podem ser adicionado a partir de <apply|menu|Document|Use
  package>.

  Do ponto de vista do programa, cada estilo corresponde a um arquivo
  <verbatim|.ts>. Os arquivos correspondentes a cada estilo são processados
  como se fossem documentos comuns, porem o editor mantém apenas o ambiente
  final de cada arquivo de estilo como o ambiente inicial do documento. Os
  arquivos de estilo são processados na ordem em que foram listados, assim
  como os arquivos de estilo usados dentro destes, recursivamente.

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
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Usar pacote>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
