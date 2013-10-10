<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Titoli standard >

  Il d.t.d. <tmdtd|header-title> fornisce i tag per i titoli. I seguenti tag
  di alto livello possono essere usati solamente quando sono inseriti in un
  tag <markup|make-title>:

  <\explain|<markup|title>>
    Specifica un titolo per il documento.
  </explain>

  <\explain|<markup|author>>
    Specifica uno o più autori per il documento.
  </explain>

  <\explain|<markup|address>>
    Specifica l'indirizzo dell'autore.
  </explain>

  <\explain|<markup|address-block>>
    Specifica un indirizzo per un autore (nel caso di indirizzi multipli).
  </explain>

  <\explain|<markup|title-email>>
    Specifica l'indirizzo di posta elettronica dell'autore.
  </explain>

  <\explain|<markup|title-date>>
    Specifica la data in cui è stato scritto l'articolo.
  </explain>

  <markup|title> e <markup|author> usano i tag <markup|header-title> e
  <markup|header-author> per specificare il titolo e l'intestazione correnti.
  Questi si possono sovrascrivere riutilizzando rispettivamente
  <markup|header-title> e <markup|header-author>. I tag descritti qui sopra
  dipendono per la loro disposizione fisica anche dai seguenti tag di basso
  livello:

  <\explain|<markup|title*>>
    Macro con un argomento che specifica il layout fisico dei titoli.
  </explain>

  <\explain|<markup|author*>>
    Macro con un argomento che specifica il layout degli autori.
  </explain>

  <\explain|<markup|address*>>
    Macro con un argomento che specifica il layout fisico degli indirizzi.
  </explain>

  <\explain|<markup|title-email*>>
    Macro con un argomento che specifica il layout fisico degli indirizzi
    email.
  </explain>

  <\explain|<markup|title-date*>>
    Macro con un argomento che specifica il layout fisico delle date di
    creazione.\ 
  </explain>

  Il <abbr|d.t.d.> <tmdtd|header-title> definisce anche il tag
  <markup|abstract> per il sommario dei documenti.

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