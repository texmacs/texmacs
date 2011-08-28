<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Supporting your system <apply|TeXmacs>>

  Supponiamo che, seguendo quanto spiegato nella sezione precedente, siate
  riusciti a scrivere una prima interfaccia per <apply|TeXmacs> di un certo
  programma applicativo. Allora è giunto il momento di includere un primo
  supporto per il vostro sistema in una distribuzione standard di
  <apply|TeXmacs>, dopodichè sarà possibile migliorarne le prestazioni.

  Dalla versione 1.0.1.5 in poi, è diventato semplice adattare una generica
  interfaccia in modo da poterla integrare direttamente in <TeXmacs>. L'idea
  è di creare una directory:

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/myplugin
  </verbatim>

  dove <verbatim|myplugin> è il nome del corrispondente plugin. Ricordiamo
  che <verbatim|$TEXMACS_HOME_PATH> è assimilato per defult a
  <verbatim|~/.TeXmacs>. Nella directory <verbatim|$TEXMACS_PATH/plugins>
  potete trovare tutti i plugin standard disponibili nella distribuzione di
  <TeXmacs> che state usando. I plugin già implementati rappresentano
  altrettanti buoni esempi da cui prendere spunto.

  La directory <verbatim|myplugin> dovrebbe contenere una struttura di
  sottodirectory analoga a quella di <verbatim|$TEXMACS_PATH>, anche se è
  possibile omettere le directory che non vengono direttamente utilizzate. In
  ogni caso deve essere fornito un file <verbatim|progs/init-myplugin.scm>
  che descriva come inizializzare il vostro plugin. Tipicamente questo file
  contiene solo una istruzione in <name|Scheme>, con una forma del genere:

  <\verbatim>
    \ \ \ \ (plugin-configure myplugin<format|next line> \ \ \ \ \ (:require
    (file-in-path "myplugin"))<format|next line> \ \ \ \ \ (:launch
    "shell-cmd")<format|next line> \ \ \ \ \ (:format "input-format"
    "output-format")<format|next line> \ \ \ \ \ (:session "Myplugin"))
  </verbatim>

  La prima istruzione è un predicato che stabilisce se il vostro plugin può
  essere utilizzato o meno su un dato sistema. Normalmente si verifica se il
  programma cui fa riferimento il plugin è disponibile nel path. Le
  istruzioni seguenti vengono eseguite solo se la richiesta precedente è
  soddisfatta. L'istruzione <verbatim|:launch> specifica che il plugin viene
  lanciato utilizzando <verbatim|shell-cmd>. Il comando <verbatim|shell-cmd>
  normalmente ha la forma <verbatim|myplugin --texmacs>. L'istruzione
  <verbatim|:format> specifica quali sono i formati utilizzati in input e in
  output. Tipicamente <verbatim|input-format> è <verbatim|verbatim> e
  <verbatim|output-format> è <verbatim|generic>. Altri formati disponibili
  sono <verbatim|scheme>, <verbatim|latex>, <verbatim|html> e <verbatim|ps>.
  L'istruzione <verbatim|:session> rende disponibili le sessioni da shell del
  plugin dal menu <apply|menu|Insert|Session|Myplugin>.

  Se tutto funziona e desiderate rendere possibile ad altri l'utilizzo del
  plugin inserendolo in una distribuzione ufficiale di <apply|TeXmacs>
  contattate Joris van der Hoeven all'indirizzo internet
  <verbatim|vdhoeven@texmacs.org>.

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

<\references>
  <\collection>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Sessione>|<with|font
      family|<quote|ss>|Myplugin>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
