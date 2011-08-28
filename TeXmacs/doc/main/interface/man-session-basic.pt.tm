<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Uso básico>

  Uma seção interativa pode ser iniciada com o menu
  <apply|menu|Insert|Session>. Uma seção consiste de uma seqüência de ambientes
  de entrada e saída, possivelmente com algum texto entre eles. Quando você
  pressiona <key|return> dentro de um ambiente de entrada de uma
  sessão, o texto dentro deste ambiente é executado e o resultado é mostrado
  no ambiente de saída.

  Quando você envia um comando para um interpretador, o aplicativo tenta
  executá-lo. Vários comandos podem ser executados ao mesmo tempo no mesmo
  documento, mas a saída só será ativa para a sessão na qual o cursor está
  localizado, e no local exato do cursor. Desta forma, recomendamos que o uso
  de buffers diferentes para execuções em paralelo.

  Para cada tipo de aplicativo externo, você pode escolher entre compartilhar
  um único processo por todas as sessões abertas, ou iniciar um processo
  diferente para cada sessão. Mais precisamente, quando você insere uma
  sessão com <apply|menu|Insert|Session|Other>, você pode especificar tanto o
  ``tipo da sessão'' (Shell, Pari, Maxima, etc.) quando um ``nome da sessão''
  (o nome padrão é ``default''). Sessões com nomes diferentes correspondem a
  processos diferentes, e sessões com o mesmo nome compartilham um mesmo
  processo.

  Para terminar o processo subjacente a uma dada sessão, você pode usar
  <apply|menu|Session|Close session>. Quando você tecla
  <key|return> na entrada de um sistema que não está conectado,
  ele será reiniciado automaticamente. Você também pode usar
  <apply|menu|Session|Interrupt execution> para interromper a execução de um
  comando. Vários aplicativos, no entanto, não tem esta funcionalidade.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Sessão>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
