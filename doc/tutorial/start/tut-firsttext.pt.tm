<TeXmacs|1.0.1.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Escrevendo um texto simples>

  Depois de dar nome ao seu documento, você pode começar a digitar seu texto.
  Mais tarde, explicaremos como digitar caracteres especiais, que são
  necessários para escrever textos não em inglês. O texto a seguir é um
  exemplo simples, que você pode tentar digitar como um exercício:

  <expand|big-figure|<apply|screenshot|simple-1.en.png>|Digitando um texto
  simples.>

  Quando você terminar de digitar, recomendamos que você salve imediatamente
  o documento, usado a entrada <with|font series|bold|save buffer> no ícone
  de menu <apply|icon|tm_save.png>, ou pressionando a tecla <key|F3>. Uma
  mensagem no rodapé deve confirmar o sucesso desta operação:

  <expand|big-figure|<apply|screenshot|simple-2.en.png>|Acabamos de salvar o
  documento, por segurança.>

  Para sua segurança o <TeXmacs> <em|salva automaticamente> seu documento a
  cada dois minutos. Se você esqueceu de salvar seu documento antes de sair
  do programa, ou se o seu computador foi desligado inadvertidamente por
  qualquer motivo, então o <TeXmacs> pergunta se você deseja recuperar as
  últimas modificações feitas ao se documento que não foram salvas
  explicitamente, tão logo você tente recarregá-lo. Novamente, uma mensagem
  no rodapé confirma o salvamento automático:

  <expand|big-figure|<apply|screenshot|simple-3.en.png>|O <TeXmacs> salva
  automaticamente seu texto a cada dois minutos.>

  Quando você termina a digitação, normalmente você deseja imprimir seu
  documento. Isto pode ser feito escolhendo <apply|menu|Print all> no ícone
  de menu <apply|icon|tm_print.png>, \ ou então pressionando a tecla
  <key|F4>. Antes de imprimir o documento, provavelmente você deve ajustar as
  propriedades da sua impressora usando <apply|menu|File|Page setup>. Você
  pode especificar qual o comando de impressão (como <with|font
  family|tt|lpr>), o tamanho do papel da sua impressora (como <with|font
  family|tt|a4> na Europa ou <with|font family|tt|letter> nos Estados Unidos)
  e a resolução da impressora (<with|font family|tt|600> é o padrão).

  Para recuperar seu documento depois de fechar o <TeXmacs>, você deve
  primeiro clicar em <apply|menu|Load buffer> no ícone de menu
  <apply|icon|tm_load.png> ou pressionar a tecla <key|F2>. Em seguida, você
  deve escolher seu arquivo usando o navegador de arquivos. Em nosso exemplo,
  o arquivo <with|font family|tt|test.tm> aparece no navegador e podemos
  carregá-lo clicando duas vezes em rápida sucessão sobre o seu nome no
  navegador.

  <expand|big-figure|<apply|screenshot|load.en.png>|Recuperando o documento
  do disco.>

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
    <associate|idx-1|<tuple|3|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-2|<tuple|3|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|gly-3|<tuple|3|?>>
    <associate|idx-3|<tuple|3|?>>
    <associate|gly-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Print all>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Page setup>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Load
      buffer>>|<pageref|idx-3>>
    </associate>
    <\associate|figure>
      <tuple|normal|Digitando um texto simples.|<pageref|gly-1>>

      <tuple|normal|Acabamos de salvar o documento, por
      segurança.|<pageref|gly-2>>

      <tuple|normal|TeXmacs automatically <with|font
      shape|<quote|italic>|autosaves> your document every two
      minutes.|<pageref|gly-3>>

      <tuple|normal|Retrieving the simple text from the
      disk.|<pageref|gly-4>>
    </associate>
  </collection>
</auxiliary>
