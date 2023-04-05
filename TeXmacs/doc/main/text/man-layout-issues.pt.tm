<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Diagrama��o>

  Como uma regra geral, o pr�prio <apply|TeXmacs> � respons�vel pela
  diagrama��o do seu texto. Assim, mesmo sem querer proibir completamente que
  voc� o fa�a, n�s n�o encorajamos que voc� tente fazer a diagrama��o do seu
  documento visualmente. Por exemplo, voc� n�o deve inserir espa�os ou linhas
  em branco para criar espa�amento vertical ou horizontal; espa�o adicional
  deve ser inserido explicitamente com <apply|menu|Format|Space>. Isto
  tornar� seu texto mais robusto, no sentido de que voc� n�o ter� que refazer
  a diagrama��o quando realizar pequenas altera��es, que podem mudar quebras
  de linhas ou p�ginas, ou grandes altera��es, como mudar o estilo do
  documento.

  V�rios tipos comandos para inser��o de espa�amento expl�cito foram
  implementados. Voc� pode inserir espa�os r�gidos com alturas ou larguras
  previamente especificadas. Espa�os horizontais n�o tem altura, e s�o
  el�sticos ou r�gidos. O comprimento de um espa�o el�stico depende da forma
  com que o par�grafo est� sendo hifenizado. Al�m disto, � poss�vel inserir
  espa�amento que considera marcas de tabula��o. Espa�os verticais podem ser
  inseridos tanto no fim quanto no come�o do par�grafo: o espa�amento
  adicional entre dois par�grafos � o m�ximo entre o espa�amento vertical
  antes do segundo par�grafo e o espa�o ap�s o primeiro par�grafo (em
  contraste com o <apply|TeX>, isto previne espa�amento desnecess�rio entre
  dois teoremas consecutivos).

  Quanto � formata��o de um par�grafo, o usu�rio pode especificar o estilo do
  par�grafo (justificado, alinhado � direita, alinhado � esquerda), as
  margens do par�grafo e os espa�oes horizontais � esquerda e � direita no
  in�cio e no final de cada par�grafo, respectivamente. O usu�rio tamb�m pode
  controlar o espa�amento entre par�grafos e entre as linhas sucessivas de
  cada par�grafo.

  Voc� pode especificar a formata��o da p�gina no menu
  <apply|menu|Document|Page>. Em primeiro lugar, voc� pode escolher a forma
  na qual as p�ginas s�o mostradas na tela do computador: escolhendo
  ``papel'' como o tipo da p�gina em <apply|menu|Document|Page|Type>, voc�
  pode ver as quebras de p�gina explicitamente. O padr�o � formatar a p�gina
  como ``papiro'', que evita a quebra de p�ginas durante a prepara��o do
  documento. O tipo ``autom�tico'' admite que o tamanho do papel � exatamente
  o mesmo da janela do <apply|TeXmacs>. As margens da p�gina e a largura do
  texto s�o especificados em <apply|menu|Document|Page|Layout>.
  Freq�entemente, � conveniente reduzir as margens para exibi��o na tela do
  computador; isto � feito em <apply|menu|Document|Page|Screen layout>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Espa�o>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>|<with|font
      family|<quote|ss>|Tipo>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>|<with|font
      family|<quote|ss>|Layout>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|P�gina>|<with|font family|<quote|ss>|Layout da
      tela>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
