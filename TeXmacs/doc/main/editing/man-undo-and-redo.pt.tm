<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Desfazendo e refazendo alterações>

  É possível desfazer gradualmente todas as mudanças que você fez em um
  documento a partir do momento em que você iniciou o <apply|TeXmacs>. Isto
  pode ser feito com o menu <apply|menu|Edit|Undo> ou usando as teclas
  <shortcut|(undo 0)> ou <shortcut|(undo 0)>. Alterações desfeitas podem ser refeitas
  com <apply|menu|Edit|Redo> ou <shortcut|(redo 0)>.

  Para economizar memória, o número de ações sucessivas que podem ser
  desfeitas é limitado normalmente a 100. É possível aumentar este número
  adicionando um comando como

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  ao seu arquivo pessoal de inicialização (ver <apply|menu|Help|Scheme>).
  Quando você especifica um número negativo como a profundidade máxima, um
  número arbitrário de alterações (sujeito à disponibilidade de memória do
  computador) de alterações podem ser desfeitas.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Desfazer>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Refazer>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ajuda>|<with|font
      family|<quote|ss>|Scheme>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
