<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambientes para teoremas e afins>

  A <abbr|d.t.d.> <tmdtd|env-theorem> fornece marcação para a diagramação de
  teoremas e similares. As mais importantes etiquetas são:

  <\explain|<markup|render-theorem>>
    Um macro para formatar ambientes similares a teoremas. O primeiro
    argumento especifica o nome do teorema, algo como ``Teorema 1.2'' e o
    segund argumento contém o corpo do teorema. Este ambiente é usado para
    teoremas definidos por <markup|new-theorem>.
  </explain>

  <\explain|<markup|render-remark>>
    Semelhante a <markup|render-theorem>, mas usado para ambientes similares
    a comentários.
  </explain>

  <\explain|<markup|render-exercise>>
    Semelhante a <markup|render-theorem>, mas para ambientes semelhates a
    exercícios.
  </explain>

  <\explain|<markup|render-proof>>
    Semelhante a <markup|render-theorem>, mas para provas. Este ambiente é
    usado principalmente para alterar o nome da prova, como em ``Fim da prova
    do teorema 1.2''
  </explain>

  <\explain|<markup|dueto>>
    Um ambiente para especificar os criadores de um teorema.
  </explain>

  <\explain|<markup|corollary*>>
    Para corolários não numerados. Este ambiente é baseado em
    <markup|render-theorem>.
  </explain>

  <\explain|<markup|proof>>
    Para provas de teoremas. Este ambiente é baseado em
    <markup|render-proof>.
  </explain>

  As etiquetas seguintes podem ser usadas para modificação dos ambientes.

  <\explain|<markup|theorem-name>>
    Um macro que controla a aparência dos nomes dos ambiente para teoremas
    <em|e> comentários. A maioria dos estilos usa negrito ou maiúsculas
    pequenas.
  </explain>

  <\explain|<markup|exercise-name>>
    Semelhante a <markup|theorem-name>, mas para exercícios.
  </explain>

  <\explain|<markup|theorem-sep>>
    O separador entre o nome do teorema ou similar e seu corpo. Em geral, um
    ponto seguido de um espaço.
  </explain>

  <\explain|<markup|exercise-sep>>
    Semelhante a <markup|theorem-sep>, mas para exercícios.
  </explain>

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