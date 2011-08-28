<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Compilare una bibliografia>

  Per il momento, <apply|TeXmacs> usa <verbatim|bibtex> per compilare le
  bibliografie. Il meccanismo per compilare automaticamente una bibliografia
  è il seguente:

  <\itemize>
    <item>Si scrive un file <verbatim|.bib> con tutti i riferimenti
    bibliografici. Questo file deve avere il formato di un file bibliografia
    standard per <apply|LaTeX>.

    <item>Usare <apply|menu|Insert|Link|Citation> e
    <apply|menu|Insert|Link|Invisible citation> per inserire le citazioni,
    che corrispondono agli elementi del file <verbatim|.bib>.

    <item>Nella posizione in cui si desidera che venga compilata la
    bibliografia, si clicchi su <apply|menu|Insert|Automatic|Bibliography>.
    Alla richiesta, si inserisca uno stile <verbatim|bibtex> (come
    <verbatim|plain>, <verbatim|alpha>, <verbatim|abbrv>, ecc.) e il proprio
    file <verbatim|.bib>.

    <item>Usare <apply|menu|Document|Update|Bibliography> per compilare la
    bibliografia.
  </itemize>

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font
      family|<quote|ss>|Citazione>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font family|<quote|ss>|Citazione
      invisible>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Automatico>|<with|font
      family|<quote|ss>|Bibliografia>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Aggiorna>|<with|font
      family|<quote|ss>|Bibliografia>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
