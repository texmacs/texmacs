<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Étude de l'exemple ``mycas''>

  La meilleure façon d'implémenter votre première interface avec
  <apply|TeXmacs> est d'examiner soigneusement l'exemple <verbatim|mycas>,
  que vous trouverez dans le répertoire <verbatim|$TEXMACS_PATH/misc/mycas>.
  Le fichier <verbatim|mycas.cpp>, dont le contenu est inclus à la fin de
  cette section, contient un programme très simple que l'on peut interfacer
  avec <apply|TeXmacs>. Pour tester ce programme, compilez-le avec :

  <\verbatim>
    \ \ \ \ g++ mycas.cpp -o mycas
  </verbatim>

  et déplacez le fichier binaire <verbatim|mycas> obtenu dans un répertoire
  connu de la variable d'environnement système PATH. Quand vous démarrerez
  <apply|TeXmacs>, un nouvel article <apply|menu|Mycas> sera intégré dans le
  menu <apply|menu|Insert|Session>.

  NdT: Si vous utilisez le port Fink de <TeXmacs>, le plus simple est de
  copier le fichier <verbatim|mycas.cpp>, situé dans le répertoire
  /sw/share/TeXmacs.../plugins/mycas/examples, dans ~/bin (créez le
  répertoire auparavant s'il n'existe pas déjà), puis compilez-le comment
  indiqué ci-dessus.

  <section|Étude du code source pas à pas>

  Étudions le code source de <verbatim|mycas> pas à pas. Tout d'abord, toutes
  les communications se font via les entrées et sorties standards à l'aide de
  tubes. Pour permettre à <apply|TeXmacs> de savoir quand les sorties système
  sont terminées, toutes les sorties doivent être encapsulées dans des blocs
  contenant trois caractères de contrôle spéciaux :\ 

  <\verbatim>
    \ \ \ \ #define DATA_BEGIN \ \ ((char) 2)<format|next line> \ \ \ #define
    DATA_END \ \ \ \ ((char) 5)<format|next line> \ \ \ #define DATA_ESCAPE
    \ ((char) 27)
  </verbatim>

  Le caractère <verbatim|DATA_ESCAPE> suivi de n'importe quel autre caractère
  <with|mode|math|c> est utilisé pour générer <with|mode|math|c>, y compris
  dans le cas où <with|mode|math|c> est l'un des trois caractères de contrôle
  mentionnés ci-dessus. Le message affiché au démarrage de la session montre
  comment utiliser <verbatim|DATA_BEGIN> et <verbatim|DATA_END> :\ 

  <\verbatim>
    \ \ \ \ int<format|next line> \ \ \ main () {<format|next line>
    \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ cout \<less\>\<less\> "Welcome to my test computer
    algebra system for TeXmacs\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "This software comes with no warranty
    whatsoever\\n";<format|next line> \ \ \ \ \ cout \<less\>\<less\> "(c)
    2001 \ by Joris van der Hoeven\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ fflush (stdout);
  </verbatim>

  La première ligne du <verbatim|main> stipule que le message de démarrage
  sera imprimé en format <space|0.2spc>verbatim<space|0.2spc>. La fonction
  <verbatim|next_input>, qui est appelée après la sortie du message, est
  utilisée pour afficher une invite et sera expliquée plus loin. Le
  <verbatim|DATA_END> final ferme le bloc de message de démarrage et indique
  à <apply|TeXmacs> que <verbatim|mycas> est en attente d'entrée. N'oubliez
  de vider la sortie standard, de façon à ce que <apply|TeXmacs> puisse
  recevoir le message dans son entier.

  La boucle principale commence par demander une saisie à partir de l'entrée
  standard :\ 

  <\verbatim>
    \ \ \ \ \ \ while (1) {<format|next line> \ \ \ \ \ \ \ char
    buffer[100];<format|next line> \ \ \ \ \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ \ \ \ \ if (strcmp (buffer, "quit") == 0)
    break;
  </verbatim>

  La sortie générée doit de nouveau figurer dans un bloc
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>.\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> "You
    typed " \<less\>\<less\> buffer \<less\>\<less\> "\\n";
  </verbatim>

  À l'intérieur de ce type de bloc, on peut envoyer récursivement d'autre
  blocs qui peuvent utiliser des formats différents. Par exemple, le code
  suivant envoie une formule <apply|LaTeX> :\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula:
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "latex:" \<less\>\<less\> "$x^2+y^2=z^2$"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> "\\n";
  </verbatim>

  Dans certains cas, il peut être utile d'envoyer directement la sortie en
  format <apply|TeXmacs> en utilisant une représentation <apply|scheme> :\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And finally a fraction
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "scheme:" \<less\>\<less\> "(frac \\"a\\" \\"b\\")"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> ".\\n";
  </verbatim>

  À la fin, il faut de nouveau envoyer <verbatim|DATA_END> et vider la sortie
  standard :\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ fflush
    (stdout);<format|next line> \ \ \ \ \ }<format|next line>
    \ \ \ \ \ return 0;<format|next line> \ \ \ }
  </verbatim>

  Notez qu'il ne faut jamais envoyer plus d'un bloc
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>. Dès que le premier bloc
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> est reçu par <apply|TeXmacs>, le
  système se met en attente d'entrée. Si vous voulez envoyer plusieurs blocs
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>, vous devez les inclure dans un
  bloc principal.

  Un <space|0.2spc>canal<space|0.2spc> spécifique est utilisé pour envoyer
  l'invite. Les canaux sont spécifiés comme des blocs
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> spéciaux :\ 

  <\verbatim>
    \ \ \ \ static int counter= 0;<format|next line><format|next line>
    \ \ \ void<format|next line> \ \ \ next_input () {<format|next line>
    \ \ \ \ \ counter++;<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    DATA_BEGIN \<less\>\<less\> "channel:prompt" \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ \ \ cout \<less\>\<less\> "Input "
    \<less\>\<less\> counter \<less\>\<less\> "] ";<format|next line> \ \ \ }
  </verbatim>

  À l'intérieur d'un canal d'invite, vous pouvez aussi utiliser des blocs
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> imbriqués. Ceci permet, par
  exemple, d'utiliser une formule comme invite. Il existe trois canaux
  standards :\ 

  <\description>
    <expand|item*|<verbatim|output>.>Canal de sortie normale par défaut.

    <expand|item*|<verbatim|prompt>.>Canal d'envoi d'une invite de saisie.

    <expand|item*|<verbatim|input>.>Canal servant à envoyer une valeur par
    défaut à la prochaine entrée.
  </description>

  <section|Sortie graphique>

  On peut envoyer des images PostScript en sortie. À supposer qu'il existe
  une image <verbatim|picture.ps> dans votre répertoire utilisateur, si vous
  insérez les lignes suivantes :

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";<format|next
    line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "ps:";<format|next line> \ \ \ \ \ \ \ fflush (stdout);<format|next line>
    \ \ \ \ \ \ \ system ("cat $HOME/picture.ps");<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> "\\n";
  </verbatim>

  à l'endroit approprié dans la boucle principale, votre image s'affichera
  dans la sortie.

  <section|Le listing complet>

  <\verbatim>
    #include \<less\>stdio.h\<gtr\><format|next line>#include
    \<less\>stdlib.h\<gtr\><format|next line>#include
    \<less\>string.h\<gtr\><format|next line>#include
    \<less\>iostream.h\<gtr\><format|next line><format|next line>#define
    DATA_BEGIN \ \ ((char) 2)<format|next line>#define DATA_END
    \ \ \ \ ((char) 5)<format|next line>#define DATA_ESCAPE \ ((char)
    27)<format|next line><format|next line>static int counter= 0;<format|next
    line><format|next line>void<format|next line>next_input () {<format|next
    line> \ counter++;<format|next line> \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "channel:prompt" \<less\>\<less\> DATA_END;<format|next
    line> \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";<format|next line>}<format|next line><format|next
    line>int<format|next line>main () {<format|next line> \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ cout \<less\>\<less\> "Welcome to my test computer algebra system
    for TeXmacs\\n";<format|next line> \ cout \<less\>\<less\> "This software
    comes with no warranty whatsoever\\n";<format|next line> \ cout
    \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ next_input ();<format|next line> \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ fflush (stdout);<format|next
    line><format|next line> \ while (1) {<format|next line> \ \ \ char
    buffer[100];<format|next line> \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ if (strcmp (buffer, "quit") == 0)
    break;<format|next line> \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "verbatim:";<format|next line> \ \ \ cout
    \<less\>\<less\> "You typed " \<less\>\<less\> buffer \<less\>\<less\>
    "\\n";<format|next line><format|next line> \ \ \ cout \<less\>\<less\>
    "And now a LaTeX formula: ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:" \<less\>\<less\>
    "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;<format|next line> \ \ \ cout
    \<less\>\<less\> "\\n";<format|next line><format|next line> \ \ \ cout
    \<less\>\<less\> "And finally a fraction ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:" \<less\>\<less\>
    "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ cout \<less\>\<less\> ".\\n";<format|next line><format|next line>
    \ \ \ next_input ();<format|next line> \ \ \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ fflush (stdout);<format|next line>
    \ }<format|next line> \ return 0;<format|next line>}
  </verbatim>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

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
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-2>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Studying the source code step by
      step><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      output><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>The complete
      listing><value|toc-dots><pageref|toc-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
