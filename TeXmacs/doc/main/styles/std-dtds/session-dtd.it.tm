<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Markup speciali per le sessioni>

  Il d.t.d. <tmdtd|session> fornisce i seguenti ambienti per le sessioni di
  computer algebra:

  <\explain|<markup|session>>
    Macro con un argomento: il corpo della sessione.
  </explain>

  <\explain|<markup|input>>
    Macro con due argomenti: un prompt e i dati in entrata (input) stessi.
  </explain>

  <\explain|<markup|output>>
    Macro con il corpo dei dati in uscita (output) come argomento.
  </explain>

  Infatti, questi ambienti sono basati sugli ambienti della forma
  <markup|<em|lan>-session>, <markup|<em|lan>-input> e
  <markup|<em|lan>-output> per ogni lingua <verbatim|<em|lan>>.

  Se gli ambienti specifici per la lingua non esistono, allora si utilizzano
  al loro posto <markup|generic-session>, <markup|generic-input> e
  <markup|generic-output>. Si raccomanda di basare gli ambienti specifici per
  la lingua sugli ambienti generici, che possono avere diverse
  implementazioni a seconda dello stile (per esempio, il pacchetto
  <tmstyle|framed-session>). A questo scopo, forniamo anche l'ambiente
  <markup|generic-output*>, che è simile a <markup|generic-output>, tranne
  per il fatto che i margini rimangono inalterati.

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