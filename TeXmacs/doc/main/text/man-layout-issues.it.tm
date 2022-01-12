<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Impaginazione>

  Come regola generale, <apply|TeXmacs> si occupa dell'impaginazione del
  testo. Pertanto, sconsigliamo l'utilizzatore di impaginare personalmente il
  proprio documento, sebbene ciò non sia proibito. Per esempio, non si
  dovrebbero inserire degli spazi o delle righe bianche con l'intento di
  sostituire spazi orizzontali o verticali tra le parole o tra le righe; gli
  spazi addizionali dovrebbero essere inseriti esplicitamente utilizzando
  <apply|menu|Format|Space>. In questo modo si renderà il documento più
  robusto nel senso che non si dovrà rimettere mano all'impaginazione se si
  apporta qualche piccola modifica, che influisce su alcune righe o alcune
  pagine, o una modifica maggiore, come un cambiamento dello stile del
  documento.

  Sono stati implementati diversi tipi di comandi di spaziatura esplicita.
  Innanzitutto, si possono inserire degli spazi rigidi con larghezza e
  altezza fissate. Gli spazi orizzontali non possiedono un'altezza e possono
  essere più o meno estensibili. La lunghezza degli spazi estensibili dipende
  da come vengono sillabate le parole nel paragrafo. Inoltre, si possono
  inserire delle tabulazioni. Gli spazi verticali possono essere inseriti sia
  all'inizio che alla fine di un paragrafo: lo spazio addizionale tra due
  paragrafi è il massimo tra lo spazio verticale dopo il primo dei due
  paragrafi e lo spazio verticale che precede il secondo (diversamente dal
  <apply|TeX>, questo evita che si creino degli spazi superflui tra due
  teoremi consecutivi).

  Per quanto riguarda l'impaginazione dei paragrafi, l'utilizzatore può
  specificarne lo stile (giustificato, allineato a sinistra, centrato o
  allineato adestra), i margini e l'indentazione a sinistra (a destra) della
  prima (ultima) riga del paragrafo. L'utilizzatore può inoltre controllare
  gli spazi tra paragrafi e tra righe successive all'interno dei paragrafi.

  Si può specificare l'impaginazione nel menu <apply|menu|Document|Page>.
  Innanzitutto, si deve specificare come verranno visualizzate le pagine
  sullo schermo: quando si seleziona ``foglio'' come tipo di pagina in
  <apply|menu|Document|Page|Type>, si vedranno esplicitamente le separazioni
  tra pagine successive. Per default, il tipo di pagina è ``papiro'', che
  evita le interruzioni tra le pagine durante la stesura del documento. Il
  tipo di pagina ``automatico'' imposta le dimensioni del foglio in modo che
  coincidano con le dimensioni della finestra su cui si sta lavorando. I
  margini della pagina e la larghezza del testo vengono specificate in
  <apply|menu|Document|Page|Layout>. Spesso, quando si lavora sullo schermo,
  conviene ridurre i margini della pagina; ciò si può fare utilizzando il
  menu <apply|menu|Document|Page|Screen layout>.

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
    <associate|page medium|papyrus>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page height|892160unit>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|page width|1269760unit>
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|<uninit>|1>>
    <associate|idx-1|<tuple|<uninit>|1>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|1>>
    <associate|idx-3|<tuple|<uninit>|1>>
    <associate|idx-4|<tuple|<uninit>|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Spazio>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Pagina>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Pagina>|<with|font
      family|<quote|ss>|Tipo>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Pagina>|<with|font
      family|<quote|ss>|Impaginazione>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Pagina>|<with|font family|<quote|ss>|Impaginazione su
      schermo>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
