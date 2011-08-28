<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Intestazioni standard >

  Il <abbr|d.t.d.> <tmdtd|header> fornisce i tag per personalizzare le
  intestazioni e le note a piè di pagina. La personalizzazione è basata
  sull'idea che possiamo specificare una <em|testo di pagina> per ogni
  pagina. Questo testo di pagina può essere, per esempio, un titolo corrente
  o il nome della sezione attuale. Il testo di pagina può dipendere dalla
  parità di una pagina e apparire in modo differente per pagine speciali come
  la pagina iniziale di un nuovo capitolo. I tag seguenti controllano
  l'impaginazione fisica dei tipi diversi di pagina:

  <\description>
    <expand|item*|<markup|start-page>>Questo tag, con testo di pagina come
    suo unico argomento, specifica l'impaginazione della prima pagina di un
    nuovo capitolo o di una sezione.

    <expand|item*|<markup|odd-page-text>>Analogo a <markup|start-page>, ma
    per l'impaginazione delle pagine dispari ordinarie.

    <expand|item*|<markup|even-page-text>>Analogo a <markup|start-page>, ma
    per l'impaginazione delle pagine pari ordinarie.
  </description>

  I tag seguenti controllano le azioni logiche relative alle intestazioni da
  eseguire quando si specfica un titolo, un autore, o quando si inizia una
  nuova sezione.

  <\description>
    <expand|item*|<markup|header-title>>Tag con un ``argomento titolo'' che
    viene utilizzato per specificare il titolo del documento.

    <expand|item*|<markup|header-author>>Tag con un ``argomento autore'' che
    viene utilizzato per specificare l'autore del documento.

    <expand|item*|<markup|header-primary>>Tag con un ``argomento nome della
    sezione'' che viene utilizzato all'inizio di ciascuna sezione di base
    (cioè <markup|chapter> per lo stile libro, o <markup|section> per lo
    stile articolo).

    <expand|item*|<markup|header-secondary>>Tag con un ``argomento nome della
    sezione'' che è utilizzato all'inizio sezione secondaria (cioè
    <markup|section> per lo stile libro, o <markup|subsection> per lo stile
    articolo).
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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|header>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|odd-page-text>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|even-page-text>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-title>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-author>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-primary>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-secondary>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection>>|<pageref|idx-14>>
    </associate>
  </collection>
</auxiliary>
