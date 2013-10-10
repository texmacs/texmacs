<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Definição de novos ambientes>

  A <abbr|d.t.d.> <tmdtd|env-manage> contém marcação de alto nível que pode
  ser usada pelo usuário para definir novos ambientes para teoremas,
  comentários, exercícios e figuras:

  <\explain|<markup|new-theorem>>
    Define um ambiente similar a um teorema. Você deve especificar o nome
    para o ambiente (como ``experiência'') e o texto correspondente (como
    ``Experiência'').
  </explain>

  <\explain|<markup|new-remark>>
    Semelhante a <markup|new-theorem>, mas para comentários.
  </explain>

  <\explain|<markup|new-exercise>>
    Semelhante a <markup|new-theorem>, mas para exercícios.
  </explain>

  <\explain|<markup|new-figure>>
    Semelhante a <markup|new-theorem>, mas para figuras (em pares para
    figuras grandes e pequenas).
  </explain>

  Esta <abbr|d.t.d.> também contém marcação de baixo nível para a definição
  destes ambientes. Na verdade, a definição de novos ambientes para teoremas
  é feita em duas etapas. Na primeira, a etiqueta <markup|new-theorem> é
  usada para especificar qual ambiente deve ser definido. Na segunda etapa,
  (imediatamente antes do documento do usuário ser definido), os ambientes
  são efetivamente definidos. Este mecanismo torna possível modifiar os
  ambientes com pacotes que são processados entre estas duas etapas. Por
  exemplo, a numeração dos teoremas é modificada desta forma.

  <\warning>
    No momento, você deve usar <markup|new-theorem> e etiquetas similares
    apenas dentro de arquivos de estilo ou pacotes pessoais. Se você usar
    <markup|new-theorem> diretamente dentro de um documento, a numeração
    poderá ser incorreta, devido ao esquema com duas etapas descrito acima.
    Esta limitação irá desaparecer tão logo seja possível especificar
    preâmbulos limpos para documentos <TeXmacs>.
  </warning>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
  </collection>
</initial>