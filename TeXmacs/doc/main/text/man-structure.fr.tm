<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Structure d'un texte>

  En général, les documents importants ont une structure. Ils sont organisés
  en chapitres, sections et sous-sections ; ils contiennent différentes
  sortes de texte, comme du texte ordinaire, des citations, des notes de bas
  de page, des théorèmes, etc... Après que vous avez choisi un
  <def-index|style de document> dans <menu|Document|Style>, <TeXmacs> se
  charge de la mise en page, telles la numérotation des sections, pages et
  théorèmes, la typographie des citations, notes en bas de page et théorèmes.

  Actuellement plusieurs styles de document ont été implémentés :
  <verbatim|<with|color|dark orange|<with|color|brown|générique>>>,
  <verbatim|<with|color|brown|article>>, <tmstyle|livre>, <tmstyle|lettre>,
  <tmstyle|examen>, <tmstyle|beamer>, <tmstyle|séminaire>, <tmstyle|source>.
  Par exemple le style article peut être utilisé pour l'écriture d'articles.
  Par ailleurs, il y a des styles pour les revues communes et d'autres pour
  des fins spéciales telle que la documentation de <TeXmacs>.

  Dès que vous avez sélectionné un style, vous pouvez organiser votre texte
  en sections (voir <menu|Insérer|Section>) et utiliser des
  <def-index|environnements> spécifiques. Par exemple un théorème, une
  proposition, une remarque... (voir <menu|Insérer|Enunciation>). Ou encore
  des listes ordinaires (voir <menu|Insérer|Liste>) ou des listes numérotées
  (voir <menu|Insérer|Énumeration>). D'autres exemples de balises fréquemment
  utilisés sont <with|color|blue|Important> (pour l'écriture de texte \S
  important \T), <with|color|blue|<samp|Nom>> ( pour l'écriture de nom de
  personnes), etc.

  Quand vous vous sentirez plus à l'aise avec <TeXmacs>, vous pourrez ajouter
  de nouveaux environnements dans un fichier de style personnalisé.
  Supposons, par exemple, que vous faites de nombreuses citations et que vous
  voulez qu'elles apparaissent en italique avec des marges gauche et droite
  d'un centimètre. Au lieu de changer manuellement les propriétés du texte et
  du paragraphe à chaque fois que vous faites une citation, il vaut mieux
  créer un environnement citation. Cela vous permettra non seulement
  d'insérer plus vite une citation, mais aussi de changer systématiquement la
  mise en page de toutes vos citations dans le document en ne changeant que
  la définition de l'environnement citation. Vous vous trouverez dans ce cas,
  lorsque vous vous rendrez compte <with|font-shape|italic|a posteriori>
  qu'il vaudrait mieux, par exemple, utiliser une police plus petite pour
  afficher les citations.

  Le respect de quelques principes généraux de l'édition de texte rend facile
  la manipulation de documents structurés à l'aide de <TeXmacs>. Un concept
  majeur est celui <em|d'environnement courant>, qui s'illustre mieux \ à
  travers un exemple.

  Supposons que nous soyons entrain de saisir un théorème classique:

  <\quote-env>
    Le théorème suivant est dû à <name|Euler>:

    <\big-focus>
      <\theorem>
        <small-focus|<math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<value|math-cursor>>>.
      </theorem>
    </big-focus>
  </quote-env>

  A la position du curseur, les boîtes bleu cyan indiquent les balises
  actives : Dans ce cas, le curseur est à la fois dans un environnement
  théorème et formule. L'environnement active le plus intime (celui de la
  \ formule <small-focus|<math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1>>
  dans notre cas) est appelé l'<em|environnement courant>.

  Le contenu du menu <samp|Focus> et de la <em|barre d'outils focus> (la
  barre d'outils la plus basse) depend fortement du contexte et est une
  fonction de l'environnement courant. Dans notre exemple, la barre d'outils
  focus contient un bouton menu <samp|Formule>; en cliquant sur
  <samp|Equation> dans ce menu, nous obtenons :

  <\quote-env>
    Le théorème suivant est dû à <name|Euler>:

    <\big-focus>
      <\theorem>
        \;

        <\big-focus>
          <\equation*>
            \<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<value|math-cursor>.
          </equation*>
        </big-focus>
      </theorem>
    </big-focus>
  </quote-env>

  Les boutons se trouvant à gauche de la barre d'outils focus permettent de
  passer rapidement d'une balise données à une autre qui lui est similaire.
  Ainsi, ils vous permettrons de parcourir rapidement toutes les formules et
  équations dans votre document. Pour plus d'information sur \S <hlink|les
  opérations d'édition structurée|../editing/man-structured-editing.fr.tm> \T
  nous renvoyons au chapitre sur <hlink|outils
  d'édition|../editing/man-editing-tools.fr.tm>.

  Une deuxième notion importante est <em|le mode d'édition courant>.
  Actuellement, il y'a cinq principaux modes d'édition : texte,
  mathématiques, programme, graphique et source. En principe, le mode
  d'édition courant peut être déterminé à partir de l'environnement courant,
  mais le mode change moins souvent que l'environnement. La <em|barre
  d'outils mode> au-dessus de la barre d'outils focus contient plusieurs
  boutons qui sont utiles dans le mode courant. Les contenus des menus
  <samp|Insérer> et <samp|Format> dépendent aussi du mode courant.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Michèle Garoche, Daouda
  Niang Diatta>

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
  </collection>
</initial>