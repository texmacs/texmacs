<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Encontrar e Substituir>

  Você pode iniciar uma busca pressionando <key|C-s> ou com
  <apply|menu|Edit|Search>. Durante a busca, a ``cadeia procurada'' é
  mostrada no lado esquerdo do rodapé. Cada caracter que você digita é
  adicionado a esta cadeia, e a próxima ocorrência da cadeia é mostrada com
  um quadro vermelho. Quando a tecla <key|C-s> é pressionada uma segunda vez
  durante uma busca, a próxima ocorrência é procurada. Um sinal sonoro indica
  que não existem mais outras ocorrências da cadeia procurada; pressionar
  <key|C-s> novamente faz com que a busca recomece do início do documento.
  Você pode usar a tecla <key|backspace> para apagar caracteres
  digitados durante a busca.

  Normalmente, o texto é procurado para a frente, começando da posição
  corrente do cursos. Você também pode procurar para trás, usando <key|C-r>.
  Durante a busca, apenas o texto no mesmo modo e na mesma linguagem vai ser
  examinado. Em outras palavras, quando você busca por <with|mode|math|x> no
  modo matemático, você não vai encontrar nenhum x comum no texto normal. É
  uma limitação atual do programa que a cadeia de busca só pode conter texto
  simples e não símbolos matemáticos ou texto estruturado mais complicado.

  Uma operação de substituição é iniciada pressionando a tecla <key|C-=> ou
  <apply|menu|Edit|Replace>. Você tem que informar qual é a cadeia procurada
  e a cadeia pela qual esta será substituída. A cada ocorrência da cadeia
  procurada, você deve informar se deseja substituir a cadeia (y), não
  substitui-la (n), ou substituir esta e todas as ocorrências posteriores
  (a). Assim como na procura, substituição é limitada ao mesmo modo e à mesma
  língua.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Procurar>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Substituir>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
