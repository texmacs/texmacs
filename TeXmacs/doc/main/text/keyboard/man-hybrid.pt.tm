<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Comandos híbridos e emulação do <LaTeX>>

  <apply|TeXmacs> permite que você entre com comandos do <apply|LaTeX>
  diretamente do teclado. Primeiro você deve digitar a tecla <key|\\> para
  entrar no modo de comando híbrido <apply|LaTeX>/<apply|TeXmacs>. Em
  seguida, você deve digitar o comando que você quer executar. Assim que você
  tenha terminado de digitar o comando, o rodapé esquerdo mostrará algo como:

  <\verbatim>
    \ \ \ \ \<less\>return\<gtr\>: action to be undertaken
  </verbatim>

  Quando você digitar <key|return>, seu comando será executado.
  Por exemplo, no modo matemático você pode criar uma fração digitando
  \ <key|\\ f r a c return>.

  Se o comando que você digitou não é um comando (reconhecido) do
  <apply|LaTeX>, então primeiramente verificamos se o comando é um macro,
  função ou ambiente (fornecido pelo arquivo de estilo) do <apply|TeXmacs>.
  Caso isto aconteça, a expansão do macro, a aplicação da função ou a criação
  do ambiente é feita (com o número correto de argumentos). Caso contrário,
  supomos que seu comando corresponde a uma variável do ambiente e usamos o
  seu valor. A tecla \ <key|\\> é sempre equivalente a um dos comandos
  <key|inactive l>, <key|inactive e>, <key|inactive a>, <key|inactive #>
  ou <key|inactive v>.

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
