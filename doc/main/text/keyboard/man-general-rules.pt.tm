<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Regras gerais para prefixos>

  Já que há tantos atalhos de teclado, é importante ter alguma maneira de
  classificá-los em várias categorias, para que sua memorização seja mais
  fácil. Como uma regra geral, atalhos do teclado que são da mesma categoria
  tem o mesmo prefixo. Os principais prefixos são:

  <\description>
    <expand|item*|<key|C-<with|mode|math|x>>>Atalhos prefixados pela tecla
    control são usados freqüentemente para comandos de edição. Eles dependem
    particularmente do ``look and feel'' selecionado em
    <apply|menu|Edit|Preferences>. Por exemplo, se você usar o look and feel
    compatível com o <name|Emacs>, atalhos da forma
    \ <key|C-<with|mode|math|x>> correspondem a comandos do <name|Emacs>,
    como <key|C-y> para colar texto.

    <expand|item*|<key|A-<with|mode|math|x>>>A tecla alt é usada para
    comandos que dependem do modo no qual você está. Por exemplo,
    <expand|kbd-text|s> produz texto <strong|realçado> no modo texto e uma
    raiz quadrada <with|mode|math|<sqrt|>> no modo matemático. Note que
    \ <key|<expand|key-escape> <expand|key-escape>> equivale a \ <key|A->.

    <expand|item*|<key|M-<with|mode|math|x>>>A tecla meta é usada para
    comandos gerais do <apply|TeXmacs>, que podem ser usados em qualquer
    modo. Por exemplo, <expand|kbd-gen|!> produz um rótulo. Ela é usada
    também para outros comandos de edição, como <key|A-w> para copiar texto
    se você usa o look and feel do <name|Emacs>. Note que
    <key|<expand|key-escape>> equivale a <key|M->.

    <expand|item*|<key|H-<with|mode|math|x>>>A tecla modificadora do usuário
    é usada para inserir símbolos especiais como letras gregas no modo
    matemático. Você pode configurar seu teclado para que a tecla caps-lock
    funcione como a tecla hiper. A tecla <key|F5> equivale a <key|H->.
  </description>

  Lembramos que as teclas modificadoras usadas para a obtenção dos prefixos
  <key|M-> e <key|H-> podem ser <apply|hyper-link|configuradas|../../config/man-config-kbd-modkeys.pt.tm>
  em <apply|menu|Edit|Preferences>.

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
    <associate|preamble|false>
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
      family|<quote|ss>|Preferências>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferências>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
