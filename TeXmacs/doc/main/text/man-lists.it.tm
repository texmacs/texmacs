<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Liste>

  Usando <apply|menu|Insert|Itemize> si può costruire una lista non numerata.
  Per indicare gli elementi della lista si può utilizzare il tag di default
  oppure si può selezionare un tag particolare come
  <with|mode|math|\<bullet\>> (puntine), <with|mode|math|<op|->> (trattini) o
  <with|mode|math|<op|\<rightarrow\>>> (frecce). Le liste possono essere
  <em|nidificate>, come si vede nella lista di esempio qui di seguito:

  <\itemize>
    <item>Primo elemento.

    <item>Sottolista:

    <\itemize>
      <item>Un sottoelemento.

      <item>Un altro sottoelemento.
    </itemize>

    <item>Ultimo elemento.
  </itemize>

  Il tag di default appare in modi differenti a seconda del livello di
  nidificazione. Al livello più esterno, si utilizza il tag
  <with|mode|math|\<bullet\>>, al secondo livello
  <with|mode|math|<op|\<circ\>>>, e così via. Si noti che, quando il cursore
  è all'interno di una lista, premendo il tasto <key|return>
  viene creato automaticamente un nuovo elemento. Se la lunghezza di un
  elemento supera la lunghezza della riga, allora si può utilizzare
  <key|S-return> per iniziare un nuovo paragrafo senza passare
  ad un altro elemento della lista.

  Le liste numerate, che sono create utilizzando <apply|menu|Insert|Enumerate>,
  si comportano in maniera analoga alle liste puntate, con l'unica differenza
  che gli elementi sono numerati. Qui di seguito vi è un esempio di una lista
  numerata creata utilizzando <apply|menu|Insert|Enumerate|I, II, III>:

  <\expand|enumerate-Roman>
    <item>Primo elemento.

    <item>Secondo elemento.

    <item>Ultimo elemento.
  </expand>

  L'ultimo tipo di liste serve per creare delle descrizioni. Vengono create
  utilizzando <apply|menu|Insert|Description> e consentono di descrivere una
  lista di concetti:

  <\description>
    <expand|item*|Gnu.>Una bestia pelosa ma gentile.

    <expand|item*|Moscerino.>Vive solamente negli zoo.
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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|III.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Lista puntata>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Lista numerata>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Lista numerata>|<with|font family|<quote|ss>|I, II,
      III>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Descrizione>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
