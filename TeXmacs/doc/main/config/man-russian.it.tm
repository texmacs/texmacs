<TeXmacs|1.0.7.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Note per utilizzatori russi e ucraini>

  Per scrivere testi in russo o in ucraino sono disponibili diverse opzioni:

  <\itemize>
    <item>scegliere Russo come lingua predefinita in
    <menu|Edit|Preferences|Language|Russian>. Se la lingua russa è
    selezionata come lingua locale, <TeXmacs> si avvierà automaticamente con
    i menu in lingua russa;

    <item>selezionare Russo per scrivere un intero documento in lingua russa
    utilizzando il menu <menu|Document|Language|Russian>;

    <item>selezionare Russo per scrivere porzioni di un testo in russo
    all'interno di un testo in altra lingua utilizzando il menu
    <menu|Format|Language|Russian>.
  </itemize>

  Se il vostro server X utilizza l'estensione xkb ed è istruito per eseguire
  lo switch tra le modalità tastiera Latina e tastiera Russa, non avete
  bisogno di introdurre modifiche. Semplicemente eseguite lo switch con la
  modalità tastiera russa e procedete. Tutto il software necessario a questo
  scopo viene normalmente incluso nelle moderne distribuzioni di GNU/Linux e
  l'estensione xkb viene attivata di default in
  <with|font-family|tt|XF86Config>. Nell'estensione xkb i caratteri russi
  sono scritti su 2 byte e le lettere russe su 0x6??. La tastiera viene
  configurata attraverso <with|font-family|tt|setxkbmap>. Quando il server X
  parte viene lanciato questo comando con il file di sistema globale
  <with|font-family|tt|Xkbmap> (usualmente questo file si trova in
  <with|font-family|tt|/etc/X11/xinit>) se esiste; e quindi, sempre se
  esiste, con il file degli utilizzatori <with|font-family|tt|~/.Xkbmap>. Un
  file <with|font-family|tt|~/.Xkbmap> può avere un aspetto del tipo

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  Questo significa che la modalità della tastiera può essere modificata
  attraverso <render-key|l-shift r-shift>. Altre scelte diffuse sono
  <prefix|C-S-> o <prefix|A-C->, si veda <with|font-family|tt|/usr/X11R6/lib/X11/xkb/>
  per ulteriori dettagli. Questo è il settaggio migliore per sistemi Linux
  moderni e nel caso in cui sia prevedibile un uso intensivo della lingua
  russa.

  In vecchi sistemi Linux l'estensione xkb è spesso disabilitata. I caratteri
  vengono scritti su 1 byte e configurati in <with|font-family|tt|xmodmap>.
  Quando il server X parte i comandi vengono lanciati attraverso
  <with|font-family|tt|Xmodmap> (tipicamente incluso in
  <with|font-family|tt|/etc/X11/xinit>), se esiste; e, sempre se esiste, con
  il file degli utilizzatori <with|font-family|tt|~/.Xmodmap>. Potete
  configurare una combinazione di tasti per la modalità di cambio della
  tastiera e utilizzare un codice in russo a 1 byte (come koi8-r) per
  lavorare in modalità Russo. Risulta comunque più agevole scaricare il
  pacchetto <with|font-family|tt|xruskb> e attivarlo all'inizio di una
  sesione di X digitando

  <verbatim| \ \ \ xrus jcuken-koi8>.

  Attraverso questo comando viene selezionato il formato jcuken (vedi oltre)
  e il codice koi8-r per la tastiera in modalità Russo. Se decidete di
  utilizzare questo settaggio per la tastiera dovete selezionare Options
  <with|mode|math|\<rightarrow\>> international keyboard
  <with|mode|math|\<rightarrow\>> russian <with|mode|math|\<rightarrow\>>
  koi8-r.

  Potete anche utilizzare il codice Windows cp1251 al posto di koi8-r anche
  se questa scelta è rara in ambiente UNIX. Se utilizzate
  <with|font-family|tt|xrus jcuken-cp1251>, selezionate cp1251 al posto di
  koi8-r.

  Tutti i metodi appena descritti richiedono alcune azioni specifiche per
  "rendere russa" la tastiera. Ciò non è difficile e per questo potete
  consultare il Cyrillic-HOWTO o, meglio, una sua versione aggiornata:

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Oltre a questo precisiamo che i metodi precedenti modificano globalmente
  tutte le applicazioni che fanno uso di X: editori di testo (emacs, nedit,
  kedit...), terminali X, <TeXmacs> ecc....

  Se avete bisogno di utilizzare raramente la lingua russa il settaggio della
  tastiera potrebbe causare più danni che benefici. Per utilizzatori
  occasionali della lingua russa <TeXmacs> implementa un metodo che non
  richiede azioni preliminari. Ovviamente questo metodo funziona solo in
  <TeXmacs> e non in altre applicazioni.

  Il modo più semplice per scrivere in russo con una tastiera standard
  americana, senza tuttavia aver apportato modifiche al software, consiste
  nel selezionare il menu <menu|Edit|Preferences|Keyboard|Cyrillic input
  method|translit>. In questo modo quando si digita una lettera dell'alfabeto
  latino verrà prodotta la lettera russa ad esso "più simile". Per ottenere
  alcune lettere russe è necessario ricorrere a combinazioni di due o tre
  lettere:<vspace|0.5fn>

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Abbreviazione>|<cell|per>|<cell|Abbreviazione>|<cell|per>>|<row|<cell|<key|text
  " e>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|accent:umlaut
  E>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|j
  var>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|J
  var>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|÷>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|×>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|ø>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|Ø>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|ù>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|Ù>>>|<row|<cell|<key|e
  var>>|<cell|<with|language|russian|font|cyrillic|ý>>|<cell|<key|E
  var>>|<cell|<with|language|russian|font|cyrillic|Ý>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|þ>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|Þ>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|ÿ>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|ß>>>>>>|Scrivere in
  cirillico su una tastiera occidentale.>

  Se desiderate ``<with|language|russian|font|cyrillic|ñõ>'' e non
  ``<with|language|russian|font|cyrillic|ø>'' dovete digitare <key|s / h>.
  Chiaramente la scelta ottimale della mappatura da lettere latine a russe
  non è unica. Potete verificare la mappatura supportata da <TeXmacs> e se
  non vi è gradita sovrascriverla in <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm>.

  Se viene selezionato jcuken al posto di translit, si ha a disposizione la
  tastiera russa ``ufficiale''. Questa tastiera ha questo nome in quanto i
  tasti \ ``qwerty'' producono ``<with|language|russian|<with|font|cyrillic|éöóêåí>''>.
  Questo metodo di input risulta più utile se avete a disposizione una
  tastiera originale russa, che ha a disposizione ulteriori lettere
  dell'alfabeto russo scritte in rosso su alcuni tasti. Volendo utilizzare
  jcuken, un effetto simile si può riprodurre incollando opportunamente degli
  adesivi con lettere dell'alfabeto russo sulla tastiera americana.

  Coloro che non dispongono di lettere russe sulla tastiera forse
  preferiscono l'impaginazione yawertyin cui il tasto ``qwerty'' produce
  ``<with|language|russian|font|cyrillic|ÿâåðòû''>. Anche qui ogni lettera
  latina viene mappata in una russa "simile", mentre altre lettere russe
  vengono prodotte con il tasto <prefix|S->-lettera. In <TeXmacs> yawerty
  viene implementato in modo un pò diverso in quanto, per convenienza, non
  vengono ridefiniti i tasti <key|$>, <render-key|¿>, <key|\\>. Le
  corrispondenti lettere russe vengono prodotte utilizzando una combinazione
  del tipo <prefix|S->-lettera.

  <tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|italian>
  </collection>
</initial>