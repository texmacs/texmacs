<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Convenzioni e nomi dei file>

  La documentazione viene organizzata in funzione dell'argomento all'interno
  di un albero di directory. Le sottodirectory della directory principale
  sono le seguenti:

  <\description>
    <expand|item*|devel>documentazione per sviluppatori;

    <expand|item*|examples>esempi di documenti in <TeXmacs>;

    <expand|item*|incoming>documentazione in stato embrionale;

    <expand|item*|main>documentazione principale;

    <expand|item*|meta>come scrivere documentazione e come compilarla.
  </description>

  Come indicazione generale si cerca di mantenere il numero di file per
  directory ragionevolmente ridotto.

  I nomi dei file nella directory principale (main) hanno la forma
  <verbatim|tipo-nome.lingua.tm>. Nelle altre directory la forma è
  <verbatim|nome.lingua.tm>. Con <verbatim|tipo> si intende un'indicazione di
  massima riguardante la caratteristica generale della documentazione come:

  <\description>
    <expand|item*|adv>documentazione per utilizzatori esperti;

    <expand|item*|man>documentazione per il manuale di <TeXmacs>;

    <expand|item*|tut>documentazione per il tutorial di <TeXmacs>.
  </description>

  La documentazione riferita ad uno stesso argomento viene tenuta unita,
  indipendentemente dal tipo. In questo modo risulta più semplice trovare
  parti di documentazione esistente relative ad uno stesso argomento. Può
  accadere inoltre che parti della documentazione inizialmente scritte per il
  tutorial vengano inserite nel manuale o viceversa. La <verbatim|lingua> in
  cui viene redatta la documentazione viene indicata da un codice di due
  lettere come, ad esempio, <verbatim|en>, <verbatim|fr> e così via. Il
  <verbatim|nome> di ciascun documento rimane invariato all'interno delle
  traduzioni nelle diverse lingue. Così, ad esempio, il documento
  <verbatim|man-keyboard.en.tm>, una volta tradotto in francese, non diviene
  <verbatim|man-clavier.fr.tm>. ma semplicemente
  <verbatim|man-keyboard.fr.tm>.\ 

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

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
