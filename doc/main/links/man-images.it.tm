<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Inserire immagini>

  Si possono includere delle immagini nel testo utilizzando il menu
  <apply|menu|Insert|Image>. Attualmente, <apply|TeXmacs> riconosce i
  seguenti formati di file: <verbatim|ps>, <verbatim|eps>, <verbatim|tif>,
  <verbatim|pdf>, <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>,
  <verbatim|xpm> e <verbatim|fig>. Qui, <verbatim|gs> (cioè ghostscript)
  viene utilizzato per rendere immagini postscript. Se ghostscript non è
  istallato sul proprio sistema, lo si può scaricare da

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  Attualmente, gli altri formati di file vengono convertiti in file
  postscript utilizzando gli script <verbatim|tiff2ps>, <verbatim|pdf2ps>,
  <verbatim|pnmtops>, <verbatim|giftopnm>, <verbatim|ppmtogif>,
  <verbatim|xpmtoppm>. Se questi script non sono disponibili sul proprio
  sistema, si consiglia di contattare il proprio amministratore di sistema.

  Per default, le immagini vengono visualizzate nelle loro dimensioni
  originali. Sono supportate le seguenti operazioni sulle immagini:

  <\itemize>
    <item>Ritagliare l'immagine seguendo un rettangolo. Il punto in basso a
    sinistra dell'immagine originale è considerato come origine per
    specificare il rettangolo di ritaglio.

    <item>Ridimensionamento di un'immagine. Quando si specifica una nuova
    larghezza, ma non l'altezza (o viceversa), l'immagine viene
    ridimensionata in modo da mantenere le proporzioni originali.

    <item>Ingrandimento dell'immagine. Un modo alternativo per ridimensionare
    un'immagine, moltiplicandone la larghezza e l'altezza per una costante.
  </itemize>

  Si è incluso anche uno script per convertire in formato encapsulated
  postscript figure che contengono formule in <LaTeX>. Per includere una
  formula <apply|LaTeX> in una figura <verbatim|xfig>, si deve inserire la
  formula come testo, selezionare un font <apply|LaTeX> e inserire un flag
  speciale nel testo.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Immagine>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
