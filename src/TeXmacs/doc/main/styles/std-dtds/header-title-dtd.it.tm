<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Titoli standard >

  Il d.t.d. <tmdtd|header-title> fornisce i tag per i titoli. I seguenti tag
  di alto livello possono essere usati solamente quando sono inseriti in un
  tag <markup|make-title>:

  <\description>
    <expand|item*|<markup|title>>Specifica un titolo per il documento.

    <expand|item*|<markup|author>>Specifica uno o più autori per il
    documento.

    <expand|item*|<markup|address>>Specifica l'indirizzo dell'autore.

    <expand|item*|<markup|address-block>>Specifica un indirizzo per un autore
    (nel caso di indirizzi multipli).

    <expand|item*|<markup|title-email>>Specifica l'indirizzo di posta
    elettronica dell'autore.

    <expand|item*|<markup|title-date>>Specifica la data in cui è stato
    scritto l'articolo.
  </description>

  <markup|title> e <markup|author> usano i tag <markup|header-title> e
  <markup|header-author> per specificare il titolo e l'intestazione correnti.
  Questi si possono sovrascrivere riutilizzando rispettivamente
  <markup|header-title> e <markup|header-author>. I tag descritti qui sopra
  dipendono per la loro disposizione fisica anche dai seguenti tag di basso
  livello:

  <\description>
    <expand|item*|<markup|title*>>Macro con un argomento che specifica il
    layout fisico dei titoli.

    <expand|item*|<markup|author*>>Macro con un argomento che specifica il
    layout degli autori.

    <expand|item*|<markup|address*>>Macro con un argomento che specifica il
    layout fisico degli indirizzi.

    <expand|item*|<markup|title-email*>>Macro con un argomento che specifica
    il layout fisico degli indirizzi email.

    <expand|item*|<markup|title-date*>>Macro con un argomento che specifica
    il layout fisico delle date di creazione.\ 
  </description>

  Il <abbr|d.t.d.> <tmdtd|header-title> definisce anche il tag
  <markup|abstract> per il sommario dei documenti.

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
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|header-title>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|make-title>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|author>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|address>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|address-block>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title-email>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title-date>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|author>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-title>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-author>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-title>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-author>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title*>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|author*>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|address*>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title-email*>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|title-date*>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|header-title>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|abstract>>|<pageref|idx-21>>
    </associate>
  </collection>
</auxiliary>
