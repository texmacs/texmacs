<TeXmacs|1.0.3.6>

<style|<tuple|generic|proclus>>

<\body>
  <\locus>
    \;

    <\make-title>
      <title|Proclus-Xlink-Joris>
    </make-title>
  </locus|<tuple|Proclus-Xlink-Joris|11|Proclus-Xlink-Alain|7|<tuple|référent>|>>

  \;

  Suggestion sur les liens : il pourrait etre utile d'avoir un menu pour
  selectionner des types ou plusieurs types de lien. En d'autre termes, au
  lieu de les rentrer par la barre d'etat, on pourrait en selectionner et
  faire en sorte que de nouveaux
 lien sont automatiquement du type indique.
  Plus tard (cf. le papier que je t'ai donner) des types de liens peuvent
  aussi faire intervenir plus que deux loci (exemple : enfin, pere, mere ).

  \;

  AH <hspace|1spc>: Oui, d'accord. Il faut pouvoir avoir le système actuel et
  celui que tu proposes. Les deux sont utiles.

  \;

  1) <locus|Creation d'identifiants uniques|<tuple|Proclus-Xlink-Joris|10|Proclus-Xlink-Alain|8|<tuple|réponse|développement>||Proclus-Xlink-Alain|13|<tuple|développement>|>>

  \ TeXmacs devrait fournir une routine de creation d'identifiant unique
  persistant. Un tel identifiant est non seulement unique pour la session
  TeXmacs courante, mais il reste unique lorsque l'on execute TeXmacs une
  deuxieme fois ou ailleurs. En d'autres termes, les identifiants uniques
  doivent etre des couples (identifiant-session, numero) au lieude
  (identifiant-fichier, numero) dans Proclus. Ici l'identifiant de session
  pourrait etre cree en fonction de 1) date, 2) pid, 3) ligne de commande 4)
  nom de l'utilisateur, 5) identificateur de la machine. Evidemment on doit
  crypter tout cela pour ne pas fournir des informations privees. A noter que
  ce genre d'informations peut aussi etre interessant
 pour garder trace de
  quelle session d'un systeme de calcul formel a calcule quoi.

  \ 2)<locus| <locus|Equivalence entre identifiants uniques ou differents
  loci avec le meme identifiant unique|<tuple|Proclus-Xlink|3|Proclus-Xlink|4|<tuple|lié>||Proclus-Xlink|5|<tuple|lié>|>>|<tuple|Proclus-Xlink-Joris|12|Proclus-Xlink-Alain|9|<tuple|développement|réponse>|>>
  Comme on l'a constate, il peut etre utile que deux loci distincts se
  comportent de la meme facon quant au liens qui leurs sont associes, quitte
  a filtrer a posteriori en utilisant des roles. Ceci peut ce faire de deux
  facons : permettre deux loci distincts
 d'avoir le meme identifiant unique
  (l'identifiant unique ne doit pas etre confondu avec un locus unique ;
  l'identifiant unique doit plutot etre considere comme un nom unique dans
  l'espace et dans le temps) ou par la creation d'une relation d'equivalence
  (par lien) entre des identifiants uniques distincts.

  Cela ne passe pas nécessairement (ce qui ne l'exclut pas) par un traitement
  à partir du nom du locus. Cela peut se faire à partir de types de lien avec
  les types "père", "fils", "frère" (on peut trouver mieux comme noms...)
  avec les héritages automatiquement que tu devines.\ 

  Je me demande si les cas qui ne relèvent pas de ce traitement relèvent
  vraiment d'un traitement par liens. Peut-être ne faut-il pas assimiler les
  glossaires, indexes, etc. à des liens. Je ne sais pas. Cela oblige à
  \ stocker de l'information qui est en fait redondante. Par exemple, les
  références à un théorème doivent-elles être traitées comme des liens quand
  les théorèmes sont numérotés?\ 

  C'est un pb que je rencontre à l'usage avec Proclus. Comment gérer ce qui
  est relié en tant que même idée<space|0.25fn>? Ce que je fais actuellement
  c'est donner un nom à cette idée, en faire un locus placé plus ou moins
  n'importe où et de créer ensuite des liens avec ce locus. Même si l'on
  pouvait retrouver cette idée facilement en lui attribuant un id.
  mnémotechnique (ce qui améliore déjà bcp les choses), cela serait-il le
  traitement le plus satisfaisant<space|0.25fn>? La plupart des liens de ce
  document sont en définitive de cette espèce...

  \ 3)<locus| Identifiants uniques et noms
  logiques|<tuple|Proclus-Xlink|6|Proclus-Xlink-Alain|10|<tuple|développement|réponse>||Proclus-Xlink-Alain|11|<tuple|développement>||Proclus-Xlink-Alain|12|<tuple|développement>||Proclus-Xlink-Alain|17|<tuple|développement>||Proclus-Xlink-Alain|19|<tuple|développement>||Proclus-Xlink-Alain|20|<tuple|développement>|>>

  \ Par definition, les identifiants uniques sont des noms non ambigues, mais
  qui peuvent etre longs et incomprehensible pour l'utilisateur. Il faut donc
  un mecanisme pour associer des noms logiques
 a des identifiants uniques,
  tout en preservant la non ambiguite. Cette non-ambiguite est preservee en
  utilisant le contexte.

  \;

  \ 4) <locus|Resolution de l'identifiant
  unique|<tuple|Proclus-Xlink-Joris|14|Proclus-Xlink-Alain|14|<tuple|développement>||Proclus-Xlink-Alain|20|<tuple|développement>||Proclus-Xlink-Alain|27|<tuple|lié>|>>

  \ La determination de l'identifiant unique en fonction du nom logique
 en
  fonction du contexte est "la resolution de l'identifiant". Quand possible,
  il faut le faire au moment de l'edition mais parfois il peut-etre
  necessaire de le faire au moment du typesetting.

  \ Par exemple, lorsque on copie un locus, on essaye d'abord de determiner
  tous les identifiants logiques presents (en utilisant le DRD pour les
  differents balises pour marquer quels sous arbres doivent etre consideres
  comme des identifiants). Ensuite, on les resoud dans le contexte original
  (garanti non ambigu). Puis, on cree un nouveau
 contexte de resolution pour
  la copie avec l'information de resolution
 de la premiere etape.
  Eventuellement, on propage cette information vers la racine du document
  (perte de localite, mais compression des donnees). En conclusion, ce
  mecanisme permet, dans la copie du locus de garder le sens original et
  unique des noms. Ces noms prennent precedence sur d'eventuelles autres
  significations du meme nom dans le document d'arrive.

  \;

  Parfois, tout ce mecanisme ne se fait pas lors des operations d'edition
  mais plutot lors de la mise en page. En effet, c'est le cas lorsque l'on
  inclut un locus dans un autre fichier. La resolution des noms doit
  continuer a se faire dans le contexte du fichier d'origine. De facon plus
  pernitieuse, lors d'une operation de copie il se peut que certains
  identifiants logiques de la selection sont seulement implicitement
  presents, c.a.d. qu'ils ne sont pas dans l'arbre, mais qu'ils apparaissent
  lors du developpement des macros. Peut-etre qu'il faudrait considerer le
  processus de developpement des macros comme un lien.

  \;

  5) <locus|Feuilles de style|<tuple|Proclus-Xlink-Joris|16>>

  Le mecanisme decrit jusqu'a present devrait etre capable de lever toute
  ambiguite quant aux creations manuelles de noms uniques et fournir un
  mecanisme non ambigu pour y acceder a partir de noms comprehensibles. Il
  reste a voir comment faire lorsque les identifiants sont construits
  automatiquement par des macros. L'approche la plus brutale est de creer un
  identifiant uniqu a chaque developpement de macro, mais en general, on
  pourrait vouloir preserver certains identifiants lors de differents
  developpements du macro.

  \ Par exemple, considere la primitive "triangle" avec trois arguments
 a, b
  et c, qui devrait affichier un triangle avec a, b et c aux sommets avec des
  liens a-\<gtr\>b, b-\<gtr\>c et c-\<gtr\>a. D'abord il faut decider ce
  qu'il se passe lorsque l'on cree deux triangles t et u.

  \ Est-ce que l'on souhaite des liens t.a-\<gtr\>u.b, u.a-\<gtr\>t.b,
  t.b-\<gtr\>u.c, etc. ou non ? On pourrait aussi vouloir creer un lien
  exterieur vers t.a. Est-ce que ceci doit toujours etre possible ou est-ce
  qu'il est legitime d'exiger un identifiant pour t afin de rendre ceci
  possible (en quel cas l'identifiant unique pour t.a pourrait etre construit
  en fonction des identifiants uniques pour t et a).

  \;

  AH <hspace|1spc>: j'aoute <hspace|1spc>:

  6)<locus| Bas<locus|e|<tuple|Proclus-Xlink-Joris|18|Proclus-Xlink-Alain|15|<tuple|développement>|>>
  de liens|<tuple|Proclus-Xlink-Joris|13|Proclus-Xlink-Alain|11|<tuple|développement>||Proclus-Xlink-Alain|13|<tuple|développement>||Proclus-Xlink-Alain|16|<tuple|lié>||Proclus-Xlink-Alain|27|<tuple|lié>|>>

  7) <locus|Copier-coller d'un locus.|<tuple|Proclus-Xlink-Joris|17|Proclus-Xlink-Alain|14|<tuple|développement>|>>

  \;

  \<gtr\> 1) Il serait bien de l'indiquer dans les menus s'il y a une source

  \<gtr\> \ \ \ ou un but actif.

  \<gtr\>

  \<gtr\> 2) Plutot que de rentrer un type direct et un type inverse,

  \<gtr\> \ \ \ on devrait pouvoir avoir des types qui font les deux a la
  fois.

  

  AH <hspace|1spc>: Oui. Il ne devrait pas être difficile d'ajouter à la
  liste des types une liste de paires. A la demande demande du type, en
  répondant par exemple "utilise*", le type "utilise" serait entré en direct
  et son type associé (par exemple "application") en type réciproque.

  

  \<gtr\> 3) Plutot que de rentrer les types un par un lors de la creation

  \<gtr\> \ \ \ d'un nouveau lien (aussi tres facile d'oublier l'entree pour

  \<gtr\> \ \ \ separer les liens directs et inverses), il vaut mieux avoir

  \<gtr\> \ \ \ un menu avec les types pour chaque nouveau lien.

  \<gtr\>

  \<gtr\> 4) S'il y a un seul lien (actif), alors y aller directement si on

  \<gtr\> \ \ \ clique dessus. Ne rien faire d'il n'y a pas de lien (actif).

  

  AH <hspace|1spc>: D'accord, mais ce doit être une option. Ou plutôt cela
  dépend de la manière dont est implémentée la modification et la suppression
  d'un lien. Elle peut se faire :

  - à partir du locus de la source (on connaît alors les types directs, mais
  ni le contenu du but ni les types indirects)

  - à partir de l'édition actuelle du lien dans le fichier annexe, et dans ce
  cas il faut pouvoir faire cette édition même quand il n'y a qu'un seul type
  et donc ne pas être automatiquement envoyé vers le but!

  Ce qui serait bien, c'est aussi une édition des liens dans un menu
  contextuel, en sélectionnant un des liens on y serait envoyé.

  \;

  \<gtr\> Encore mieux que la premiere suggestion, tu pourrais :

  \<gtr\> 1) creer un lien

  \<gtr\> 2) specifier les types du lien dans le menu des types (toggles).

  \<gtr\> De la meme facon, si tu veux editer un lien a posteriori, tu
  pourrais :

  \<gtr\> 1) specifier le source et le but du lien a editer

  \<gtr\> 2) specifier les types du lien dans le menu des types.

  \<gtr\> Cela donne une interface plus uniforme pour la creation et
  l'edition.

  AH <hspace|1spc>: Si je comprends bien, ce que tu vises c'est : ne pas
  imposer d'ordre dans les étapes de la création d'un lien. Oui, c'est
  certainement utile...

  \;
</body>

<\initial>
  <\collection>
    <associate|absolute-name|Proclus-Xlink-Joris>
    <associate|language|french>
    <associate|locus-num|18>
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
    sur|commentaire|contre-exemple|cas particulier|question|thème|analyse|Français|Latin|Anglais|Allemand|bla|blou|réponse>>
    <associate|proclus-type-list|<tuple|référence|citation|application|lié|réciproque|utilise|généralisation|référent|exemple|développement|idée|définition|théorème|commentaire
    sur|commentaire|contre-exemple|cas particulier|question|thème|analyse|Français|Latin|Anglais|Allemand|bla|blou|réponse>>
  </collection>
</initial>