<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Generazione automatica del contenuto>

  Il d.t.d. <tmdtd|std-automatic> consente la generazione automatica di
  contenuti ausiliari come indici e bibliografie, e inoltre la presentazione
  dei contenuti stessi. I tag seguenti vengono utilizzati per le
  bibliografie:

  <\explain|<markup|cite>>
    Una funzione con un numero arbitrario di argomenti. Ciascun argomento è
    una citazione corrispondente ad un elemento in un file BiB-<TeX>. Le
    citazioni sono visualizzate nella stessa maniera in cui sono riportate
    nella bibliografia e forniscono anche degli hyperlink ai riferimenti
    corrispondenti. Se non si genera la bibliografia, le citazioni appaiono
    come punti di domanda.
  </explain>

  <\explain|<markup|nocite*>>
    Analogo a <markup|cite>, ma le citazioni non sono visualizzate nel testo
    principale.
  </explain>

  <\explain|<markup|bibitem*>>
    Funzione che specifica come visualizzare un elemento nella bibliografia.
  </explain>

  I tag seguenti sono utilizzati per compilare gli indici:

  <\explain|<markup|toc-main-1>>
    Funzione con un argomento per creare voci primordiali nell'indice. Questa
    funzione può, per esempio, essere usata quando un libro è costituito di
    molte parti.
  </explain>

  <\explain|<markup|toc-main-2>>
    Funzione con un argomento per creare una voce principale nell'indice.
    Generalmente si usa questa funzione per i capitoli.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Funzione con un argomento per creare una voce normale nell'indice. Questa
    funzione è usata spesso per le sezioni.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Analogo a <markup|toc-normal-2> per le voci meno importanti come le
    sottosezioni.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Analogo a <markup|toc-normal-3> per le voci assai meno importanti come
    sotto-sottosezioni.
  </explain>

  <\explain|<markup|toc-small-1>>
    Usata per le voci non molto importanti come i paragrafi (può essere
    ignorata).
  </explain>

  <\explain|<markup|toc-small-2>>
    Usata per le voci assai meno importanti come i sottoparagrafi
  </explain>

  <\explain|<markup|toc-dots>>
    Il separatore tra una voce nell'indice e il numero di pagina
    corrispondente. Per default, usiamo i punti orizzontali.
  </explain>

  I tag seguenti sono utilizzati per gli indici analitici:

  <\explain|<markup|index>>
    Funzione con un argomento <var|x>, che inserisce <var|x> nell'indice come
    una voce principale.
  </explain>

  <\explain|<markup|subindex>>
    Funzione con due argomenti <var|x> e <var|y>, che inserisce <var|y>
    nell'indice come sottovoce di <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Funzione con tre argomenti <var|x>, <var|y> e <var|z>, che inserisce
    <var|z> nell'indice come sottovoce di <var|y>, la quale è a sua volta
    sottovoce di <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Funzione con quattro argomenti <var|key>, <var|how>, <var|range>,
    <var|entry>, che è documentata nella sezione riguardante <hlink|la
    generazione degli indici|../../links/man-index.it.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Questa funzione prende un argomento <var|key>, che indica come ordinare
    le voci, e la voce attuale <var|entry>. Non viene generato alcun numero
    di pagina.
  </explain>

  <\explain|<markup|index-1>>
    Macro con una voce dell'indice e un numero di pagina, che viene usata per
    inserire nell'indice una voce principale.
  </explain>

  <\explain|<markup|index-1*>>
    Analogo a <markup|index-1>, ma senza il numero di pagina.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (con <math|n> tra <math|1> e <math|5>): macro con una voce dell'indice e
    un numero di pagina, che è utilizzata per inserire nell'indice una voce
    di livello <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Analogo a <markup|index-<math|n>>, ma senza il numero di pagina.
  </explain>

  <\explain|<markup|index-dots>>
    Macro che produce i punti tra una voce dell'indice e il numero (o i
    numeri) di pagina corrispondente.
  </explain>

  I tag seguenti sono utilizzati per i glossari:

  <\explain|<markup|glossary>>
    Funzione che inserisce il suo unico argomento nel glossario.
  </explain>

  <\explain|<markup|glossary-dup>>
    Per creare un numero di pagina addizionale per una voce che è già stata
    inserita.
  </explain>

  <\explain|<markup|glossary-explain>>
    Funzione per inserire una voce del glossario con la relativa spiegazione.
  </explain>

  <\explain|<markup|glossary-line>>
    Per inserire una voce del glossario snza alcun numero di pagina.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro per inserire una voce del glossario e il corrispondente numero di
    pagina.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro inserire una voce del glossario, la sua spiegazione e il suo numero
    di pagina.
  </explain>

  <\explain|<markup|glossary-dots>>
    Macro che produce i punti tra una voce del glossario e i corrispondenti
    numeri di pagina.
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
    <associate|language|italian>
  </collection>
</initial>