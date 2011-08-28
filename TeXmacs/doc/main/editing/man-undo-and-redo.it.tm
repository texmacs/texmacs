<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Annullare e ripetere>

  È possibile annullare gradualmente le modifiche eseguite in un documento
  dal momento in cui è stato lanciato <apply|TeXmacs>. Ciò può essre fatto
  tramite <apply|menu|Edit|Undo> o utilizzando le combinazioni di tasti
  <shortcut|(undo 0)> o <shortcut|(undo 0)>. Le modifiche annullate possono essere
  ripristinate mediante <apply|menu|Edit|Redo> o <shortcut|(redo 0)>.

  Per limitare l'uso della memoria, il numero di azioni successive che
  possono essere annullate è per default limitato a 100. È comunque possibile
  aumentare questo numero aggiungendo una linea di comando come la seguente\ 

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  nel proprio file di inizializzazione (si veda
  <apply|menu|Help|Manual|Customizing TeXmacs>). Quando si specifica un
  numero negativo come dimensione massima di annullamenti, qualunque numero
  di azioni può essere annullato.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Annulla>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Rifai>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Aiuto>|<with|font
      family|<quote|ss>|Manuale>|<with|font family|<quote|ss>|Personalizzare
      TeXmacs>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
