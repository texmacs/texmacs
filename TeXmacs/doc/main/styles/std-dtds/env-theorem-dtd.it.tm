<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambienti di tipo teorema>

  Il d.t.d. <tmdtd|env-theorem> fornisce i tag per la disposizione
  tipografica degli ambienti di tipo teorema. I tag più importanti sono\ 

  <\explain|<markup|render-theorem>>
    Macro per visualizzare gli ambienti di tipo teorema. Il primo argomento
    specifica il nome del teorema, come \ ``Teorema 1.2'' e il secondo
    argomento contiene il corpo del teorema. Questo ambiente e usato per gli
    ambienti definiti con <markup|new-theorem>.
  </explain>

  <\explain|<markup|render-remark>>
    Analogo a <markup|render-theorem>, ma per gli ambienti di tipo nota.
  </explain>

  <\explain|<markup|render-exercise>>
    Analogo <markup|render-theorem>, ma per gli ambienti di tipo esercizio.
  </explain>

  <\explain|<markup|render-proof>>
    Analogo a <markup|render-theorem>, ma per le dimostrazioni. Questo
    ambiente è usato soprattutto per personalizzare il nome di una
    dimostrazione, come in ``Fine della dimostrazione del teorema 1.2''.\ 
  </explain>

  <\explain|<markup|dueto>>
    Un ambiente che può essere usato per specificare gli autori di un
    teorema.
  </explain>

  <\explain|<markup|corollary*>>
    Per corollari non numerati. Questo ambiente è basato su
    <markup|render-theorem>.
  </explain>

  <\explain|<markup|proof>>
    Per le dimostrazioni dei teoremi. Questo ambiente è basato su
    <markup|render-proof>.
  </explain>

  I tag seguenti possono essere utilizzati per ulteriori personalizzazioni
  degli ambienti.

  <\explain|<markup|theorem-name>>
    Una macro che controlla l'aspetto dei nomi degli ambienti di tipo teorema
    <em|e> nota. La maggior parte degli stili utilizza il grassetto o lettere
    maiuscole piccole.
  </explain>

  <\explain|<markup|exercise-name>>
    Analogo a <markup|theorem-name>, ma per gli esercizi.
  </explain>

  <\explain|<markup|theorem-sep>>
    Il separatore tra il nome di un ambiente di tipo teorema o di tipo nota e
    il il suo corpo principale. Per default, questo è un punto seguito da uno
    spazio.
  </explain>

  <\explain|<markup|exercise-sep>>
    Analogo a <markup|theorem-sep>, ma per gli esercizi.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia Gecchelin|Andrea
  Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>