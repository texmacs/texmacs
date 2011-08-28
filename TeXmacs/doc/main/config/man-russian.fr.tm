<TeXmacs|1.0.7.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Notes pour les utilisateurs russes et ukrainiens>

  Pour saisir des textes en russe (ou en ukrainien), vous avez le choix entre
  :

  <\itemize>
    <item>Choisir le russe comme langage par défaut avec
    <menu|Edit|Preferences|Language|Russian>. \ <TeXmacs> charge
    automatiquement les menus russes si le russe est désignée comme votre
    langue par défaut dans votre système.

    <item>Choisir le russe pour un document donné avec
    <menu|Document|Language|Russian>.

    <item>Choisir le russe pour une partie de texte à l'intérieur d'un
    document avec <menu|Format|Language|Russian>.
  </itemize>

  Si votre serveur X utilise l'extension xkb et qu'il sait passer du mode
  latin au mode russe, vous n'avez pratiquement rien à faire. Il suffit que
  vous passiez en mode clavier russe. L'ensemble des logiciels nécessaires
  pour effectuer cette opération est inclus dans les distributions récentes
  de Linux et l'extension xkb est activée par défaut dans
  <with|font-family|tt|XF86Config>. Avec l'extension xkb, les caractères sont
  écrits sur 2 bytes et les caractères russes démarrent à 0x6??. Le clavier
  est configuré par <with|font-family|tt|setxkbmap>. Lors de son lancement, X
  envoie cette commande avec le fichier global <with|font-family|tt|Xkbmap>
  situé en général, s'il existe, dans <with|font-family|tt|/etc/X11/xinit> ;
  puis il envoie, s'il existe, le fichier utilisateur
  <with|font-family|tt|~/.Xkbmap>. Le fichier <with|font-family|tt|~/.Xkbmap>
  contient, en général, la ligne suivante :

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  Cela signifie qu'on peut changer de mode clavier avec <key|l-maj. temp.
  r-maj. temp>. On peut aussi choisir <key|ctrl majuscule temporaire> ou
  <key|ctrl alt>, voir <with|font-family|tt|/usr/X11R6/lib/X11/xkb/> pour de
  plus amples informations. C'est la méthode la plus répandue pour changer de
  clavier sur les systèmes Linux récents, dans le cas où vous devez
  fréquemment écrire en russe.

  Sur les systèmes Linux plus anciens, l'extension xkb est, en général,
  désactivée. Les caractères sont écrits sur 1 byte et sont configurés à
  l'aide de <with|font-family|tt|xmodmap>. Lors de son lancement, X envoie
  cette commande avec le fichier global <with|font-family|tt|Xmodmap> situé
  en général, s'il existe, dans <with|font-family|tt|/etc/X11/xinit> ; puis
  il envoie, s'il existe, le fichier utilisateur
  <with|font-family|tt|~/.Xmodmap>. Vous pouvez définir la combinaison de
  touches pour changer de mode clavier et choisir un encodage russe (par
  exemple koi8-r) sur 1 byte lorsque vous êtes en mode russe. Il est plus
  facile, néanmoins, de télécharger le progiciel <with|font-family|tt|xruskb>
  et de lancer :

  <verbatim| \ \ \ xrus jcuken-koi8>

  au démarrage de votre session X. Cela vous permet de disposer du clavier
  jcuken (voir plus loin) et de l'encodage koi8-r en mode russe. Si vous
  utilisez cette configuration de clavier, vous devez sélectionner
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|Koi8-r>.

  Vous pouvez aussi utiliser l'encodage Windows cp1251 au lieu de koi8-r,
  bien que cela soit rare sous UNIX. Si vous utilisez
  <with|font-family|tt|xrus jcuken-cp1251>, choisissez Cp1251 au lieu de
  Koi8-r.

  Toutes les méthodes décrites ci-dessus demandent un minimum de préparation
  pour ``russifier'' le clavier. Ce n'est pas très difficile, voir
  Cyrillic-HOWTO ou mieux, sa dernière version :\ 

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Toutes les méthodes ci-dessus ont aussi un effet sur toutes les
  applications X : éditeurs de texte \ (emacs, nedit, kedit...), xterms,
  <TeXmacs>, etc...

  Si vous écrivez en russe de façon très épisodique, il vaut mieux ne pas
  configurer de clavier. <TeXmacs> fournit, dans ce cas, des méthodes de
  saisie en russe qui ne demandent aucune préparation. Ces méthodes n'ont
  d'effet que dans <TeXmacs>.

  Le moyen le plus simple de saisir du russe sur un clavier standard US sans
  logiciel particulier est de choisir \ <menu|Edit|Preferences|Keyboard|Cyrillic
  input method|translit>. La saisie d'un caractère latin produira alors le
  caractère russe ``le plus proche''. Pour saisir certains caractères russes,
  vous devrez utiliser des combinaisons de deux ou 3 caractères :

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Raccourcis>|<cell|Pour>|<cell|Raccourcis>|<cell|Pour>>|<row|<cell|<key|text
  " e>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|accent:umlaut
  E>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|j
  tab>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|J
  tab>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|÷>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|×>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|ø>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|Ø>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|ù>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|Ù>>>|<row|<cell|<key|e
  tab>>|<cell|<with|language|russian|font|cyrillic|ý>>|<cell|<key|E
  tab>>|<cell|<with|language|russian|font|cyrillic|Ý>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|þ>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|Þ>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|ÿ>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|ß>>>>>>|Saisie d'un texte
  cyrillique sur un clavier roman.>

  Si, au contraire, vous voulez obtenir, par exemple,
  ``<with|language|russian|font|cyrillic|ñõ>'' et non pas
  ``<with|language|russian|font|cyrillic|ø>'', vous devez saisir <key|s / h>.
  Bien sûr, ce n'est pas la seule possibilité pour faire correspondre des
  caractères latins à des caractères russes. Regardez les correspondances
  fournies dans <TeXmacs> et, si quelque chose ne vous convient pas,
  changez-le dans <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm>.

  Si vous choisissez jcuken plutôt que translit, vous obtiendrez le clavier
  de machine à écrire russe ``standard''. On l'appelle ainsi car les touches
  ``qwerty'' donnent ``<with|language|russian|font|cyrillic|éöóêåí>''. Cette
  méthode de saisie est très pratique si vous avez un clavier d'origine russe
  standard, clavier qui possède des caractères russes supplémentaires peints
  en rouge sur les touches. On peut obtenir la même chose en collant des
  transparents avec des caractères russes peints en rouge sur un clavier US
  standard). De même, si vous pouvez saisir un texte russe en aveugle,
  utilisez ce clavier.

  Les personnes qui ne possèdent pas de clavier russe utilisent, en général,
  le clavier yawerty, qui permet d'obtenir
  ``<with|language|russian|font|cyrillic|ÿâåðòû>'' en saisissant ``qwerty''.
  Chaque caractère est mis en correspondance avec un caractère russe
  ``similaire'' ; on obtient certains caractères russes spécifiques
  avec<key|majuscule temporaire>-nombre. <TeXmacs> fournit un clavier yawerty
  légèrement modifié, car les touches <key|$>, <render-key|¿> et <key|\\>,
  qui servent par ailleurs dans <TeXmacs>, ne sont pas redéfinies. Les
  caractères russes correspondant avec la combinaison de touches
  <key|majuscule temporaire>-nombre.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Michèle Garoche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
    <associate|preamble|false>
  </collection>
</initial>