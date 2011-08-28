<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macros, funções e variáveis do ambiente>

  As principais combinações de teclas que você deve conhecer para escrever
  arquivos de estilo são as seguintes:

  <\description>
    <expand|item*|<key|M-=>>cria uma nova atribuição. O primeiro
    argumento é o nome do novo comando e o segundo uma expressão.

    <expand|item*|<key|M-w>>permite mudar localmente uma ou mais
    variáveis do ambiente. Comandos `with' são da forma 
    <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<r\
    sub|n>\|b\<rangle\>>, onde os <with|mode|math|x<rsub|i>> são os nomes das
    variáveis, os <with|mode|math|a<rsub|i>> seus valores locais, e
    <with|mode|math|b> o texto ao qual aplica-se a variável local.

    <expand|item*|<key|M-m>>cria um macro. Argumentos para o macro 
    podem ser inseridos com a tecla <key|tab>.  

    <expand|item*|<key|M-f>>cria uma função. Argumentos para a função
    podem ser inseridas com a tecla  <key|tab>.

    <expand|item*|<key|inactive #>>obtém o valor de um argumento de um macro.

    <expand|item*|<key|inactive v>>obtém o valor de uma variável do ambiente.

    <expand|item*|<key|inactive e>>expande o macro com zero ou mais
    argumentos.

    <expand|item*|<key|inactive a>>aplica a função a zero ou mais
    argumentos.
  </description>

  Mais precisamente, durante a expansão de um macro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> criado por
  <key|inactive e>, acontecem a seguinte seqüência:

  <\itemize>
    <item>Se <with|mode|math|a> não é uma cadeia de caracteres ou um macro,
    então <with|mode|math|a> é avaliado uma vez. Disto resulta ou um nome de
    um macro ou uma macro expressão <with|mode|math|f>.

    <item>Se foi obtido um nome de um macro, então <with|mode|math|f> é 
    substituido pelo valor da variável do ambiente <with|mode|math|f>.
    Se, após isto, <with|mode|math|f> ainda não é uma macro expressão,
    então retornamos <with|mode|math|f>.

    <item>Sejam <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> os argumentos
    de <with|mode|math|f> e <with|mode|math|b> seu corpo (argumentos
    desnecessários são descartados; uma cadeia de caracteres vazia é tomada
    como o valor padrão dos argumentos não fornecidos). Então cada
    <with|mode|math|x<rsub|i>> é substituido para cada 
    <with|mode|math|y<rsub|i>> em <with|mode|math|b> e este valor é retornado.

  </itemize>

  Funções são similares a macros, com a exceção de que argumentos de uma
  aplicação de função são avaliados e não podem ser editados diretamente
  (primeiro você precisa desativar a aplicação da função, editar os
  argumentos, e reativá-la). Além disso, 
  <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> são agora variáveis do
  ambiente local, às quais são dados os valores de 
  <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>>. Estas variáveis locais
  não são lembradas quando uma função retorna uma função que envolve estas
  variáveis.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven &
  Ramiro Brito Willmersdorf>

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
    <associate|language|english>
  </collection>
</initial>
