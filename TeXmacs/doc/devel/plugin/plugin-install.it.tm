<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Installare e usare un plugin>

  Di solito un plugin, che chiamiamo genericamente <verbatim|<em|mioplugin>>,
  viene reso disponibile in alcuni siti web in formato tarball binario con il
  nome

  <\verbatim>
    \ \ \ \ <em|mioplugin>-<em|version>-<em|architecture>.tar.gz
  </verbatim>

  Se si installa <TeXmacs> nella directory <verbatim|$TEXMACS_PATH>, allora
  si dovrà scompattare questo tarball nella directory
  <verbatim|$TEXMACS_PATH/plugins>, utilizzando il comando

  <\verbatim>
    \ \ \ \ tar -zxvf <em|mioplugin>-<em|version>-<em|architecture>.tar.gz
  </verbatim>

  In tal modo verrà creata la sottodirectory <verbatim|<em|mioplugin>> in
  <verbatim|$TEXMACS_PATH/plugins>. Dopo aver fatto ripartire <TeXmacs> il
  plugin dovrebbe essere automaticamente riconosciuto. Siete comunque pregati
  di leggere la documentazione relativa al plugin che volete utilizzare per
  capirne bene il funzionamento.

  <\remark>
    Se non avete eseguito personalmente l'installazione di <TeXmacs> o se non
    avete accesso diretto alla directory <verbatim|$TEXMACS_PATH>, potete
    scompattare il tarball direttamente nella directory
    <verbatim|$TEXMACS_HOME_PATH/plugins>. Infatti ricordiamo che
    <verbatim|$TEXMACS_HOME_PATH> è per default <verbatim|$HOME/.TeXmacs>.
    Dopo aver fatto ripartire <TeXmacs> il plugin dovrebbe, anche questa
    volta, essere automaticamente riconosciuto.
  </remark>

  <\remark>
    Se un plugin viene distribuito come tarball sorgente del tipo
    <verbatim|<em|mioplugin>-<em|version>-src.tar.gz> allora è necessario,
    prima di rilanciare <TeXmacs>, compilare il codice sorgente del plugin.
    In relazione ai diversi plugin (leggere le istruzioni) ciò di norma viene
    fatto attraverso il comando

    <\verbatim>
      \ \ \ \ cd <em|mioplugin>; make
    </verbatim>

    o

    <\verbatim>
      \ \ \ \ cd <em|mioplugin>; ./configure; make
    </verbatim>
  </remark>

  <\remark>
    Per l'aggiornamento di un plugin è sufficiente rimuovere la versione
    vecchia contenuta in <verbatim|$TEXMACS_PATH/plugins> o in
    <verbatim|$TEXMACS_HOME_PATH/plugins>, utilizzando il comando

    <\verbatim>
      \ \ \ \ rm -rf <em|mioplugin>
    </verbatim>

    e quindi reinstallare la versione aggiornata.
  </remark>

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>
