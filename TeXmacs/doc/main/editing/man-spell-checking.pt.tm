<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Verificação ortográfica>

  Se o programa <verbatim|ispell> estiver instalado no seu sistema, então
  você pode usá-lo para verificar seu texto em relação a palavras digitadas
  incorretamente. Você pode pressionar <shortcut|(spell-start)> ou
  <apply|menu|Edit|Spell>. Note que você pode ter que verificar se os
  dicionários correspondentes às línguas dos seus textos estão instalados no
  seu sistema; isto é comum para o Inglês.

  Quando você inicia o verificador ortográfico (em todo o texto ou apenas no
  trecho selecionado do documento), para cada palavra digitada errada você
  será perguntado sobre qual ação a ser tomada, sendo que no rodapé estão
  indicadas as opções disponíveis:

  <\description>
    <expand|item*|a)>Aceite a palavra como está no texto e todas as suas
    ocorrências futuras.

    <expand|item*|r)>Substitua a palavra por uma correção que você digitará a
    seguir.

    <expand|item*|i)>Indica que palavra está correta e que a mesma deve ser
    inserida em seu dicionário pessoal.

    <expand|item*|1-9)>Escolhe uma das sugestões para substituir a palavra.
  </description>

  Perceba que o <verbatim|ispell> só corrige palavras digitadas
  incorretamente. Erros gramaticais não são detectados.

  Quando você inicia o verificador ortográfico, ele usará o dicionário da
  língua ativa no posição atual do cursor (ou no início da região
  selecionada). Apenas o texto nesta língua será verificado. Se seu documento
  usa várias línguas diferentes, o verificador deve ser usado para cada uma
  delas.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Ortografia>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
