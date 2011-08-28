<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Editando sessões interativas>

  Dentro dos campos de entrada de sessões interativas, as teclas de cursor
  tem significado especial: quando você move o cursor para cima ou para
  baixo, você move a entrada para os campos de entrada anteriores ou
  posteriores. Quando você usa as teclas de movimento para a esquerda ou
  direita, você nunca deixa o campo de entrada atual, você precisa usar o
  mouse para mover o cursor lateralmente para fora de um campo de entrada.

  Os menus <apply|menu|Session|Insert fields> e <apply|menu|Session|Remove
  fields> fornecem algumas facilidades para editar os campos de entrada,
  saída e textos. A maioria das operações aplica-se diretamente a um par
  correspondente de campos de entrada e saída. Opcionalmente, um campo com um
  texto explicativo pode ser associado com um campo de entrada com
  <apply|menu|Session|Insert fields|Insert text field>. Atalhos do teclado
  para inserção de campos são <shortcut|(structured-insert-up)> (insere acima) a
  <shortcut|(structured-insert-down)> (insere abaixo). Atalhos de teclado para campos
  de texto/entrada/saída correspondentes são \ <shortcut|(structured-remove-left)>
  (remove para trás) e <shortcut|(structured-remove-right)> (remove os campos
  correntes).

  É possível criar ``sub-sessões'' usando <apply|menu|Session|Insert
  fields|Fold input field> ou <shortcut|(structured-insert-right)>. Neste caso, o campo
  corrente de texto, entrada ou saída torna-se o corpo de uma sub-sessão
  ``desdobrada''. Esta sub-sessão consiste de um texto explicativo junto a
  uma seqüência de campos de entrada e saída. Subsessões podem ser
  ``dobradas'' e ``desdobradas'' com \ <shortcut|(fold)> e
  <shortcut|(unfold)>, respectivamente. A formatação gráfica das
  subsessões na tela é muito atraente quando se usa o pacote
  <tmpackage|varsession> que está em <apply|menu|Document|Use
  package|Program>.

  Outras operações de edição úteis para campos de text/entrada/saída são
  <apply|menu|Session|Remove fields|Remove all output fields>, que é útil
  para criar sessões de demonstração que serão executadas mais tarde, e
  <apply|menu|Session|Split session>, que pode ser usada para dividir uma
  seção em várias partes para inclusão em um artigo.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdor>

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Sessão>|<with|font
      family|<quote|ss>|Insert fields>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sessão>|<with|font
      family|<quote|ss>|Remove fields>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sessão>|<with|font
      family|<quote|ss>|Insert fields>|<with|font family|<quote|ss>|Insert
      text field>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sessão>|<with|font
      family|<quote|ss>|Insert fields>|<with|font family|<quote|ss>|Fold
      input field>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|varsession>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Usar pacote>|<with|font
      family|<quote|ss>|Programa>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sessão>|<with|font
      family|<quote|ss>|Remove fields>|<with|font family|<quote|ss>|Remove
      all output fields>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sessão>|<with|font
      family|<quote|ss>|Split session>>|<pageref|idx-8>>
    </associate>
  </collection>
</auxiliary>
