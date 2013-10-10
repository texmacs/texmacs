<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambienti per oggetti fluttuanti>

  Il <abbr|d.t.d.> <tmdtd|<abbr|>env-float> fornisce i tag per gli oggetti
  fluttuanti. Il tag seguente è l'unico di alto livello:

  <\explain|<markup|footnote>>
    crea una nota a piè di pagina.
  </explain>

  I seguenti tag di basso livello possono essere utilizzati per definire
  ambienti di alto livello per le figure e le tabelle: <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> e <markup|small-table>:

  <\explain|<markup|render-small-figure>>
    Una macro per visualizzare una figura piccola. Gli argomenti sono un nome
    breve (come ``figura'' o ``tabella'') per la lista di figure, il suo nome
    reale (come ``Figura 2.3'' o ``Tabella <no-break>5''), la figura stessa e
    una didascalia.
  </explain>

  <\explain|<markup|render-big-figure>>
    Una variante di <markup|render-small-figure> per visualizzare una figura
    grande.
  </explain>

  I tag seguenti possono essere utilizzati per personalizzare l'aspetto del
  testo attorno alle figure, alle tabelle e alle note a piè di pagina:

  <\explain|<markup|figure-name>>
    Una macro che controlla l'aspetto del testo ``Figura''. Per default,
    viene utilizzato il grassetto.
  </explain>

  <\explain|<markup|figure-sep>>
    Il separatore tra la figura , il suo numero e la didascalia. Per default,
    questo è un punto seguito da uno spazio.
  </explain>

  <\explain|<markup|footnote-sep>>
    Il separatore tra il numero della nota e il testo. Per default, questo è
    un punto seguito da uno spazio.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Lucia Gecchelin|Andrea
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
    <associate|language|italian>
  </collection>
</initial>