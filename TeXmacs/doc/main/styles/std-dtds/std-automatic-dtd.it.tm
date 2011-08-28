<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generazione automatica del contenuto>

  Il d.t.d. <tmdtd|std-automatic> consente la generazione automatica di
  contenuti ausiliari come indici e bibliografie, e inoltre la presentazione
  dei contenuti stessi. I tag seguenti vengono utilizzati per le
  bibliografie:

  <\description>
    <expand|item*|<markup|cite>>Una funzione con un numero arbitrario di
    argomenti. Ciascun argomento è una citazione corrispondente ad un
    elemento in un file BiB-<TeX>. Le citazioni sono visualizzate nella
    stessa maniera in cui sono riportate nella bibliografia e forniscono
    anche degli hyperlink ai riferimenti corrispondenti. Se non si genera la
    bibliografia, le citazioni appaiono come punti di domanda.

    <expand|item*|<markup|nocite*>>Analogo a <markup|cite>, ma le citazioni
    non sono visualizzate nel testo principale.

    <expand|item*|<markup|bibitem*>>Funzione che specifica come visualizzare
    un elemento nella bibliografia.
  </description>

  I tag seguenti sono utilizzati per compilare gli indici:

  <\description>
    <expand|item*|<markup|toc-main-1>>Funzione con un argomento per creare
    voci primordiali nell'indice. Questa funzione può, per esempio, essere
    usata quando un libro è costituito di molte parti.

    <expand|item*|<markup|toc-main-2>>Funzione con un argomento per creare
    una voce principale nell'indice. Generalmente si usa questa funzione per
    i capitoli.

    <expand|item*|<markup|toc-normal-1>>Funzione con un argomento per creare
    una voce normale nell'indice. Questa funzione è usata spesso per le
    sezioni.

    <expand|item*|<markup|toc-normal-2>>Analogo a <markup|toc-normal-2> per
    le voci meno importanti come le sottosezioni.

    <expand|item*|<markup|toc-normal-3>>Analogo a <markup|toc-normal-3> per
    le voci assai meno importanti come sotto-sottosezioni.

    <expand|item*|<markup|toc-small-1>>Usata per le voci non molto importanti
    come i paragrafi (può essere ignorata).

    <expand|item*|<markup|toc-small-2>>Usata per le voci assai meno
    importanti come i sottoparagrafi

    <expand|item*|<markup|toc-dots>>Il separatore tra una voce nell'indice e
    il numero di pagina corrispondente. Per default, usiamo i punti
    orizzontali.
  </description>

  I tag seguenti sono utilizzati per gli indici analitici:

  <\description>
    <expand|item*|<markup|index>>Funzione con un argomento <var|x>, che
    inserisce <var|x> nell'indice come una voce principale.

    <expand|item*|<markup|subindex>>Funzione con due argomenti <var|x> e
    <var|y>, che inserisce <var|y> nell'indice come sottovoce di <var|x>.

    <expand|item*|<markup|subsubindex>>Funzione con tre argomenti <var|x>,
    <var|y> e <var|z>, che inserisce <var|z> nell'indice come sottovoce di
    <var|y>, la quale è a sua volta sottovoce di <var|x>.

    <expand|item*|<markup|index-complex>>Funzione con quattro argomenti
    <var|key>, <var|how>, <var|range>, <var|entry>, che è documentata nella
    sezione riguardante <apply|hyper-link|la generazione degli
    indici|../../links/man-index.it.tm>.

    <expand|item*|<markup|index-line>>Questa funzione prende un argomento
    <var|key>, che indica come ordinare le voci, e la voce attuale
    <var|entry>. Non viene generato alcun numero di pagina.

    <expand|item*|<markup|index-1>>Macro con una voce dell'indice e un numero
    di pagina, che viene usata per inserire nell'indice una voce principale.

    <expand|item*|<markup|index-1*>>Analogo a <markup|index-1>, ma senza il
    numero di pagina.

    <expand|item*|<markup|index-<with|mode|math|n>>>(con <with|mode|math|n>
    tra <with|mode|math|1> e <with|mode|math|5>): macro con una voce
    dell'indice e un numero di pagina, che è utilizzata per inserire
    nell'indice una voce di livello <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Analogo a
    <markup|index-<with|mode|math|n>>, ma senza il numero di pagina.

    <expand|item*|<markup|index-dots>>Macro che produce i punti tra una voce
    dell'indice e il numero (o i numeri) di pagina corrispondente.
  </description>

  I tag seguenti sono utilizzati per i glossari:

  <\description>
    <expand|item*|<markup|glossary>>Funzione che inserisce il suo unico
    argomento nel glossario.

    <expand|item*|<markup|glossary-dup>>Per creare un numero di pagina
    addizionale per una voce che è già stata inserita.

    <expand|item*|<markup|glossary-explain>>Funzione per inserire una voce
    del glossario con la relativa spiegazione.

    <expand|item*|<markup|glossary-line>>Per inserire una voce del glossario
    snza alcun numero di pagina.

    <expand|item*|<markup|glossary-1>>Macro per inserire una voce del
    glossario e il corrispondente numero di pagina.

    <expand|item*|<markup|glossary-2>>Macro inserire una voce del glossario,
    la sua spiegazione e il suo numero di pagina.

    <expand|item*|<markup|glossary-dots>>Macro che produce i punti tra una
    voce del glossario e i corrispondenti numeri di pagina.
  </description>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia
  Gecchelin|Andrea Centomo>

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
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-automatic>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nocite*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|bibitem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-1>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-2>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-1>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-2>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-dots>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-34>>
    </associate>
  </collection>
</auxiliary>
