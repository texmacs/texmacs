<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Objetos dinâmicos>

  Alguns objetos mais complexos podem ter vários estados durante a edição de
  um arquivo. Exemplos de <em|objetos dinâmicos> são rótulos e referências,
  porque a aparência de uma referência depende de um número determinado
  dinamicamente. Muitos outros exemplos de marcação dinâmica podem ser
  encontrados na documentação sobre a <apply|hyper-link|escrita de arquivos
  de estilo|../../../devel/style/keyboard/style-kbd.pt.tm>.

  Na inserção de um objeto dinâmico como um rótulo usando <shortcut|(make-label)>,
  o estado padrão do objeto é <em|inativo>. O estado inativo permite que você
  digite informação que é relevante ao objeto dinâmico, tal como o nome do
  rótulo, neste caso particular. Alguns objetos dinâmicos podem ter um número
  arbitrário de parâmetros, e novos parâmetros podem ser inseridos usando-se
  a tecla <key|var>.

  Ao terminar de digitar a informação relevante do objeto dinâmico, você pode
  digitar <key|return> para <em|ativar> o objeto. Um objeto
  dinâmico ativo pode ser desativado colocando o cursor logo após o objeto e
  digitando <key|backspace>.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
  </collection>
</references>
