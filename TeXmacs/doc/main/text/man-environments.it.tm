<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambienti>

  Analogamente ai tag di contesto, gli ambienti sono utilizzati per
  caratterizzare delle porzioni del testo che hanno un significato
  particolare. Tuttavia, mentre i <apply|hyper-link|tag di
  contesto|man-content-tags.it.tm> generalmente racchiudono piccole porzioni
  di testo, gli ambienti spesso racchiudono delle parti che si sviluppano su
  più paragrafi. Degli ambienti usati frequentemente in matematica sono
  <markup|teorema> e <markup|dimostrazione>, come nell'esempio seguente:

  <\theorem>
    Non esistono interi positivi <with|mode|math|a>, <with|mode|math|b>,
    <with|mode|math|c> e <with|mode|math|n> con
    <with|mode|math|n\<geqslant\>3>, tali che
    <with|mode|math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    Non ho lo spazio sufficiente per scrivere qui la dimostrazione.
  </proof>

  Si può attivare un ambiente utilizzando il menu
  <apply|menu|Insert|Environment>. Altri ambienti che forniscono un aspetto
  simile ai teoremi sono <markup|propositione>, <markup|lemma>,
  <markup|corollario>, <markup|assioma>, <markup|definizione>. Si può
  utilizzare la macro <markup|dueto> (attivata utilizzando <key|\\ d u e t o
  return>) per specificare la persona (o le persone) a cui
  appartiene il teorema, come nel seguente esempio

  <\theorem>
    <dueto|Pitagora>In circostanze opportune, vale
    <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  Altri ambienti utilizzati frequentemente con un rendering simile a quello
  dei teoremi, ma che non enfatizzano il testo in essi racchiuso, sono
  <markup|importante>, <markup|nota>, <markup|esempio>, <markup|avviso>,
  <markup|esercizio> e <markup|problema>. Gli altri ambienti <markup|testo
  semplice>, <markup|codice>, <markup|citazione>, <markup|citazione (più
  paragrafi)> e <markup|verso> possono essere utilizzati per introdurre testi
  con molti paragrafi o del codice, delle citazioni o delle poesie.

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
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-20|<tuple|2|?>>
    <associate|idx-10|<tuple|2|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|2|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|2|?>>
    <associate|idx-15|<tuple|2|?>>
    <associate|idx-16|<tuple|2|?>>
    <associate|idx-17|<tuple|2|?>>
    <associate|idx-18|<tuple|2|?>>
    <associate|idx-19|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|teorema>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dimostrazione>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Ambiente>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|propositione>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|lemma>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corollario>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|assioma>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|definizione>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|importante>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nota>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|esempio>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|avviso>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|esercizio>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|problema>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|testo semplice>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|codice>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|citazione>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|citazione (più paragrafi)>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verso>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
