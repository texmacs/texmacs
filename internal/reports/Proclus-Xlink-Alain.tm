<TeXmacs|1.0.3.6>

<style|<tuple|generic|proclus>>

<\body>
  \;

  Je commence par essayer des dégager quelques différences de conception
  entre Xlink et Proclus. Mon impression en regardant à nouveau Xlink et ton
  adaptation c'est que Xlink est "orienté loci" alors que Proclus est
  "orienté liens". Ils semblent à cet égard duaux.

  Je précise...

  Ton système me semble offrir ses possibilités à partir des loci et en
  particulier d'être bien pour gérer les loci de même type ou l'héritage : il
  privilégie le copiage et la reproduction des loci. L'accent mis sur les id
  non uniques et la surchage en témoigne. L'exemple (traduction du manuel)
  que tu m'as donné me semble en fait bien relever de cela (j'y reviendrai).

  Proclus s'applique au contraire aux liens (à partir de leurs types) : le
  traitement (l'équivalent des roles) doit se faire dans une "feuille de
  liens" qui déterminera le traitement à appliquer suivant les types des
  liens. Mais sans modifier le contenu des liens. Les types deviennent des
  paramètres (auxquels d'autres peuvent être ajoutés comme tu le notais
  (notamment l'environnement)) pour des opérations qui peuvent être traitées
  globalement sur le document (faire ceci pour tous les liens de type 
  application  sauf si la source etc.) ou locale sur un locus considéré.
  Dans Proclus, la surchage porte sur les types.

  Cela peut se voir aussi au niveau des conflits que chacun des systèmes a à
  gérer. Dans le tien, ce que tu dois considérer ce sont les conflits qui
  viennent des loci, dans Proclus, les conflits viennent (viendront...) de la
  gestion d'un lien qui a plusieurs types.

  <locus|Mais peut-être ton système peut-il néanmoins permettre de reproduire
  des loci et des liens à la Proclus. Ce serait bien de tirer cela au clair.
  Cela renvoie aux questions des noms mnémotechniques et de la base de
  données de liens.|<tuple|Proclus-Xlink-Alain|27|Proclus-Xlink-Joris|14|<tuple|thème>||Proclus-Xlink-Joris|13|<tuple|thème>|>>

  J'ai aussi toujours, peut-être à tort, l'impression que Xlink reste
  tributaire de son contexte de création lié au web, c'est-à-dire qu'il est
  fait d'une part pour permettre de faire des choses même quand les buts sont
  dans des docs dont on n'a pas les droits d'écriture et surtout qu'il est
  conçu pour créer des documents dynamiques mais en fait "figés", sur 
  lesquels on n'est plus en train de travailler. Autrement dit, Xlink est
  toujours fondé sur un modèle séparant auteur-lecteur ou encore le temps de
  création du document et le temps de lecture du document : l'auteur peut
  créer des choses sophistiquées et très dynamiques, mais quand il le fait il
  n'est pas dans de bonnes conditions de lecture de son document, quant au
  lecteur il ne peut pas écrire. Il y a une distinction nette, une
  succession, entre le moment de la composition du document et le moment de
  sa lecture. Il me semble là aussi que ton exemple avec le Manuel illustre
  malgré tout un peu ceci (dans la mesure où tu veux éviter au traducteur de
  créer un locus parce que cela est assez compliqué, tu ne veux donc qu'il
  n'agisse qu'à un certain niveau même si ici le traducteur est bcp plus
  qu'un lecteur, mais il n'est qu'un lecteur du point de vue des liens).

  Proclus au contraire est fondé sur l'idée de brouiller cette opposition et
  permet de créer des liens au moment de la lecture, ce moment étant
  indiscernable de celui de l'écriture. Les liens dans Proclus sont à la fois
  des outils de travail et de lecture (d'où le nom "Proclus"...). L'usage des
  liens dans ton système (mais je m'en fais peut-être une mauvaise idée) me
  semble assez similaire à celui des environnements : l'environnement est
  très puissant mais prédéfini dans une feuille de style (dans un langage peu
  convivial, i.e. l'utilisateur n'est pas vraiment sensé trop y toucher) mais
  ensuite on peut en créer des instances très aisément (c'est pour cela que
  Proclus peut éventuellement être reproduit à l'intérieur de celui-ci).

  \;

  Enfin, tu mets l'accent sur des liens <em|logiques>, dans Proclus les liens
  servent à stocker une information relationnelle. Mais peut-être est-ce
  seulement parce que tu mets l'accent sur la partie qui n'est pas disponible
  dans Proclus.

  \;

  J'essaye maintenant de voir <locus|<strong|les possibilités offertes par
  les noms mnémotechniques>|<tuple|Proclus-Xlink-Alain|12|Proclus-Xlink|6|<tuple|thème>|>>,
  sans considérer pour l'instant la possibilité qu'ils soient communs à
  plusieurs loci (ce serait bien que tu complètes la liste)

  1) créer un lien sans se déplacer vers le but.

  2) créer un lien sans avoir le droit d'écriture dans le doc. contenant le
  but.

  3) créer un lien avec un doc. absent lors de la création du lien.

  4) <locus|créer un lien avec un doc. sur lequel plusieurs personnes
  travaillent (c'est un pb que je rencontre dans l'usage de proclus au sein
  du groupe de travail sur l'histoire de la géométrie algébrique réelle) : pb
  de conflit des identifiants absolus si les personnes travaillent sur des
  copies du doc. Peut-être ton système complexe d'identifiant absolu pourrait
  résoudre cela. C'est à voir (il y a toujours le pb de la fusion des loci
  créés dans deux copies différentes du même doc. sur une même partie de
  texte).|<tuple|Proclus-Xlink-Alain|13|Proclus-Xlink-Joris|13|<tuple|thème>||Proclus-Xlink-Joris|10|<tuple|thème>|>>\ 

  5) permet l'identification de locus différents, là encore, avec les
  avantages des points 1)-4).

  \;

  Mais tout n'est peut-être pas aussi idyllique... car :

  1) il faut tout de même connaître les noms mnémotechniques (sans perdre les
  avantages de 1)-4)). Y a-t-il alors une différence fondamentale (dans
  l'implémentation) entre connaître les noms mnémotechniques et connaître les
  identifiants uniques?

  2) <locus|Risque de ne pas pouvoir créer de lien réciproque ou alors de le
  faire au prix de perdre les avantages de 1)-4). Cela nous renvoie à la base
  de liens.|<tuple|Proclus-Xlink-Alain|16|Proclus-Xlink-Joris|13|<tuple|référence>|>>

  \;

  Je distinguerai a) le confort lors de la création de liens apporté par les
  noms mnémotechniques de b) les fonctionalités nouvelles qu'ils permettent
  d'avoir.

  a) me semble tout à fait compatible avec Proclus tel qu'il est implémenté.

  b) Ce serait bien que tu donnes des exemples concrets de liens qui ne te
  semblent pas être couverts par Proclus, non pas dans sa version actuelle,
  mais dans ses évolutions possibles ;-). Ton exemple du triangle ne me
  convainc pas bcp...

  \;

  J'essaye maintenant de voir <locus|les possibilités offertes par
  <strong|les identifiants non uniques>|<tuple|Proclus-Xlink-Alain|15|Proclus-Xlink-Joris|18|<tuple|thème>|>>
  (ce serait bien que tu complètes la liste) <hspace|1spc>:

  <\itemize-minus>
    <item>identifier des loci sans créer de lien <space|0.25fn>(gain de
    robustesse) <space|0.25fn>;
  </itemize-minus>

  Mais alors toujours la même question <hspace|1spc>: en quoi cela concerne
  les liens<space|0.25fn>? Ne pourrait-il pas s'agir d'une sorte d'extension
  du sytème de liens mais autonome par rapport à celui-ci. Un système
  d'identification entre loci étant défini, il y aurait des options
  d'extension des liens tenant compte des identifications de loci.

  <section|Réponses diverses>

  A lire à partir du fichier <locus|Proclus-Xlink-Joris|<tuple|Proclus-Xlink-Alain|7|Proclus-Xlink-Joris|11|<tuple|référence>|>>.

  <locus|Sur l'intérêt pour le calcul formel je n'ai rien à dire. C'est toi
  qui peut mieux apprécier si cela relève d'un traitement par les liens. Pour
  le reste, cela me semble inutilement compliqué si c'est seulement pour ne
  pas utiliser de \ nom absolu pour un document. Même dans ton système un nom
  absolu revient tout simplement à tjrs considérer le document comme un locus
  et à lui attribuer un label et ensuite à utiliser ce label comme nom
  absolu. Tu risques d'avoir besoin de ce locus par défaut, ne serait-ce que
  pour préciser la langue du document par exemple. Tu disposes donc sans pb
  d'un nom absolu. Qu'y a-t-il ensuite de si compliqué à générer des
  identifiants automatiques ? Il suffit d'avoir quelque part dans le doc. une
  variable qui contient le numéro du dernier id créé comme dans toute base de
  données.<next-line>Dans Proclus ce locus pourrait toujours être ("nom abs",
  0). On pourrait y accéder par le menu (car il ne contiendrait aucun texte)
  et il faudrait le protéger contre l'effacement.|<tuple|Proclus-Xlink-Alain|8|Proclus-Xlink-Joris|10|<tuple|question|thème>|>>

  \;

  \;

  \<gtr\> 2) Equivalence entre identifiants uniques ou differents loci avec
  le meme identifiant unique

  <locus|Cela ne passe pas nécessairement (ce qui ne l'exclut pas) par un
  traitement à partir du nom du locus. Cela peut se faire à partir de types
  de lien avec les types "père", "fils", "frère" (on peut trouver mieux comme
  noms...) avec les héritages automatiques que tu devines.<next-line>Je me
  demande si les cas qui ne relèvent pas de ce traitement relèvent vraiment
  d'un traitement par liens. Peut-être ne faut-il pas assimiler les
  glossaires, indexes, etc. à des liens. Je ne sais pas... Cela oblige à
  \ stocker de l'information qui est en fait redondante. Par exemple, les
  références à un théorème doivent-elles traitées comme des liens quand les
  théorèmes sont numérotés et que leur application est signalée de manière
  stéréotypée dans les démonstrations?<next-line>C'est un pb que je rencontre
  à l'usage avec Proclus. Comment gérer ce qui est relié en tant que même
  idée<space|0.25fn>? Ce que je fais actuellement c'est donner un nom à cette
  idée, en faire un locus placé plus ou moins n'importe où et de créer
  ensuite des liens avec ce locus. Même si l'on pouvait retrouver cette idée
  facilement en lui attribuant un id. mnémotechnique (ce qui améliore déjà
  bcp les choses), cela serait-il le traitement le plus
  satisfaisant<space|0.25fn>? La plupart des liens de ce document sont en
  définitive de cette espèce<space|0.25fn>! Il s'agit alors simplement de
  définir des champs typés. C'est bien sûr très simple. On pourrait mettre
  autant de types que l'on voudrait et ensuite on définirait le traitement
  que l'on veut des champs de même type en définissant la portée de ce
  traitement (quels fichiers<space|0.25fn>?). Des types logiques plus
  sophistiqués comme tu les envisages peuvent être utiles. Mais s'agit-il
  toujours de liens<space|0.25fn>?<next-line>Je me demande si l'on n'en
  arrive pas à une distinction entre <hspace|1spc>:<next-line>identification
  (liaison) par le locus <hspace|1spc>: c'est essentiellement ton
  système.<next-line>identification (liaison) par un lien <hspace|1spc>:
  c'est essentiellement Proclus.|<tuple|Proclus-Xlink-Alain|9|Proclus-Xlink-Joris|12|<tuple|thème|question>|>>\ 

  \;

  <locus|Ce qui suit peut être aussi à considérer (ce n'est pas une
  nécessairement une alternative) : pouvoir utiliser n'importe quelle partie
  d'un locus comme identifiant sans avoir préalablement à la déclarer comme
  tel. Par exemple, un théorème qui est dans un locus contient "somme de
  carrés", il pourrait être intéressant de pouvoir créer un lien en appelant
  "somme de carrés" (ou un ensemble de mots : "somme, carrés, Pythagore"), le
  système se débrouillant pour ensuite déterminer l'identifiant (absolu...)
  qui correspond (et bien sûr d'indiquer quand il ne trouve pas ou s'il y a
  plusieurs réponses). Cela me semble intéressant à l'usage et évite d'avoir
  à se souvenir exactement si le nom mnémotechnique était "th pyth",
  "Pythagore", "Théorème de Pythagore", "Pyth", etc... Qu'il s'agisse d'un
  théorème pourrait être aussi indiqué (comme tu l'envisageais dans les
  attribus). Cette solution me semble assez intéressante car relativement
  facile à implémenter, elle offre certains des avantages des noms
  mnémotechniques tout en évitant d'avoir à gérer un système de
  correspondance et de conflits entre les noms
  mnémotechniques.|<tuple|Proclus-Xlink-Alain|10|Proclus-Xlink|6|<tuple|thème|question>|>>

  <locus|Ces considérations sur le nom mnémotechnique me font voir que
  Proclus force actuellement les liens à être réciproques et oblige notamment
  que le document contenant le but soit actuellement accessible
  (présent+droits d'écriture). C'est incontestablement un défaut. Il n'y a
  pas de raison de forcer la réciprocité et il est surtout utile de pouvoir
  créer des liens avec un doc. soit absent soit pour lequel je n'ai pas les
  droits d'écriture. Le seul moyen de faire cela me semble effectivement
  d'avoir des noms mnémotechniques distincts de l'identifiant.<next-line>Il
  faudrait (et il est possible) ajouter à Proclus la possibilité de créer des
  liens unidirectionnels avec un identifiant mnémotechnique pour le but. Mais
  il est bien clair que l'on perd dans ces conditions le lien réciproque (ce
  qui est embêtant), ce qui ne sera peut-être pas le cas dans ton
  système.<next-line>Si en plus on veut des liens réciproques, il faut une
  base de liens annexes.<next-line>Et là il faut que tu m'expliques comment
  un document avec lequel des liens ont été créés en son absence saura, une
  fois présent bien sûr..., les liens crées avec
  lui.|<tuple|Proclus-Xlink-Alain|11|Proclus-Xlink|6|<tuple|thème>||Proclus-Xlink-Joris|13|<tuple|thème>|>>\ 

  \;

  <locus|Je me demande s'il ne faudrait pas définir un  copier-lier . Cela
  consisterait à choisir entre liens de type  père,  fils ,  frère , le
  texte affiché étant par défaut le même. <next-line>Un copier normal
  oublierait le locus.<next-line>Et basta<space|0.25fn>!<next-line>Il serait
  intéressant (au moins dans Proclus) d'avoir une opération  consolidation 
  <hspace|1spc>: tous les liens enregistrés sur un frère ou un père seraient
  alors effectivement enregistrés dans le locus considéré afin de pouvoir
  préserver les liens même si l'un des documents contenant un frère ou un
  père disparaît. C'est aussi certainement un aspect sur lequel ton système
  est plus solide, d'un autre côté, quand les liens sont stockés dans les
  loci on risque moins de perdre <em|toute> l'information relative aux liens
  (qui est souvent à moitié conservée dans le lien réciproque stocké
  ailleurs) mais on risque plus souvent d'en perdre un peu en détruisant un
  locus...|<tuple|Proclus-Xlink-Alain|14|Proclus-Xlink-Joris|14|<tuple|thème>||Proclus-Xlink-Joris|17|<tuple|thème>|>>

  \;

  <locus|<strong|Implémentation des noms mnémotechniques sur
  Proclus>.<next-line>De la même manière que pour les abs.
  names.<next-line>Création d'un dictionnaire dans un fichier global annexe.
  Les noms mnémotechniques n'interviennent qu'en surface <hspace|1spc>: ils
  ne sont pas stockés dans le locus, les liens sont toujours enregistrés à
  partir des identifiants absolus.|<tuple|Proclus-Xlink-Alain|17|Proclus-Xlink|6|<tuple|thème>|>>

  \;

  <locus|Concernant l'exemple que tu m'as donné du manuel dans les
  différentes langues.<next-line>En fait, je ne vois plus bien son intérêt...
  Je ne vois pas ce qu'il y a de compliqué pour le traducteur à créer un lien
  qui pourrait n'être d'ailleurs ici qu'un lien html (le lien inverse n'étant
  pas très intéressant). <next-line>Dans Proclus, le traducteur créerait tout
  simplement un lien de type "référence" vers la traduction en \ y renvoyant
  éventuellement par un nom mnémotechnique si le document n'est pas
  accessible (car c'est effectivement qch d'utile). Au pire, il peut créer un
  lien de type "traduction" et ensuite au niveau de la "feuille de liens" il
  sera fait ce qu'il faut pour trouver la bonne langue. Mais il n'est pas
  difficile pour un traducteur de connaître sa langue (!) et cela peut être
  mis parmi les types par défaut.<next-line>Je comprends que tu veux stocker
  à part la structure des liaisons commune aux différentes langues de
  traduction, est-ce vraiment très important? Pourrais-tu me donner un
  exemple où cela serait vraiment intéressant?<next-line>Encore une fois, le
  gain possible que je vois dans ton système c'est de pouvoir créer sans
  avoir accès au doc. contenant le but des liens réciproques, alors qu'il est
  clair que dans l'implémentation actuelle de Proclus cela n'est pas possible
  (mais peut être fait en renonçant au lien
  réciproque).|<tuple|Proclus-Xlink-Alain|19|Proclus-Xlink|6|<tuple|thème>|>>

  <\locus>
    Cela nécessite des identifiants mnémotechniques qui requièrent
    eux-mêmes<next-line>

    de pouvoir connaître ces identifiants sans avoir accès au doc.<next-line>

    de retrouver le locus à partir de cet identifiant<next-line>

    J'aimerais mieux comprendre comment tu fais cela.<next-line>

    \ Ton système supporte-t-il bien par exemple l'exportation de seulement
    quelques-uns de tes\ 

    documents liés<space|0.25fn>?<next-line>

    Tu va devoir exporter ta base de liens. Il n'y en qu'une
    seule<space|0.25fn>? <abbr|etc.>

    \;
  </locus|<tuple|Proclus-Xlink-Alain|20|Proclus-Xlink|6|<tuple|thème>||Proclus-Xlink-Joris|14|<tuple|thème>|>>

  <locus|Bilan sur le copier-coller<next-line>Actuellement, tu peux copier un
  locus dans le doc. d'où il provient<space|0.25fn>; toutes les copies seront
  des locus frères (il ne faut pas changer le
  texte).|<tuple|Proclus-Xlink-Alain|22>>
</body>

<\initial>
  <\collection>
    <associate|absolute-name|Proclus-Xlink-Alain>
    <associate|language|french>
    <associate|locus-num|27>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|proclus-active-types|<tuple|référence|citation|application|lié|réciproque|utilise|généralisation|référent|exemple|développement|idée|définition|théorème|commentaire
    sur|commentaire|contre-exemple|cas particulier|question|thème|analyse|Français|Latin|Anglais|Allemand|bla|blou|réponse|réponose|question>>
    <associate|proclus-type-list|<tuple|référence|citation|application|lié|réciproque|utilise|généralisation|référent|exemple|développement|idée|définition|théorème|commentaire
    sur|commentaire|contre-exemple|cas particulier|question|thème|analyse|Français|Latin|Anglais|Allemand|bla|blou|réponse|réponose|question>>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Réponses
      diverses> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <pageref|auto-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>