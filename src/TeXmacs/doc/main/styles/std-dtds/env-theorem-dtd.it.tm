<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambienti di tipo teorema>

  Il d.t.d. <tmdtd|env-theorem> fornisce i tag per la disposizione
  tipografica degli ambienti di tipo teorema. I tag più importanti sono\ 

  <\description>
    <expand|item*|<markup|theorem*>>Macro per visualizzare gli ambienti di
    tipo teorema. Il primo argomento specifica il nome del teorema, come
    \ ``Teorema 1.2'' e il secondo argomento contiene il corpo del teorema.
    Questo ambiente e usato per gli ambienti definiti con
    <markup|newtheorem>.

    <expand|item*|<markup|remark*>>Analogo a <markup|theorem*>, ma per gli
    ambienti di tipo nota.

    <expand|item*|<markup|exercise*>>Analogo <markup|theorem*>, ma per gli
    ambienti di tipo esercizio.

    <expand|item*|<markup|proof*>>Analogo a <markup|theorem*>, ma per le
    dimostrazioni. Questo ambiente è usato soprattutto per personalizzare il
    nome di una dimostrazione, come in ``Fine della dimostrazione del teorema
    1.2''.\ 

    <expand|item*|<markup|dueto>>Un ambiente che può essere usato per
    specificare gli autori di un teorema.

    <expand|item*|<markup|corollary*>>Per corollari non numerati. Questo
    ambiente è basato su <markup|theorem*>.

    <expand|item*|<markup|proof>>Per le dimostrazioni dei teoremi. Questo
    ambiente è basato su <markup|proof*>.
  </description>

  I tag seguenti possono essere utilizzati per ulteriori personalizzazioni
  degli ambienti.

  <\description>
    <expand|item*|<markup|theoremname>>Una macro che controlla l'aspetto dei
    nomi degli ambienti di tipo teorema <em|e> nota. La maggior parte degli
    stili utilizza il grassetto o lettere maiuscole piccole.

    <expand|item*|<markup|exercisename>>Analogo a <markup|theoremname>, ma
    per gli esercizi.

    <expand|item*|<markup|theoremsep>>Il separatore tra il nome di un
    ambiente di tipo teorema o di tipo nota e il il suo corpo principale. Per
    default, questo è un punto seguito da uno spazio.

    <expand|item*|<markup|exercisesep>>Analogo a <markup|theoremsep>, ma per
    gli esercizi.
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
    <associate|language|english>
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
      magenta>|env-theorem>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|remark*>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercise*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corollary*>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof*>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremname>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercisename>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremname>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremsep>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercisesep>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremsep>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
