<TeXmacs|1.0.7.17>

<style|tmdoc>

<\body>
  <tmdoc-title|Preferences utilisateur>

  Pour une utilisation optimale, vous pouvez souhaiter configurer <TeXmacs>
  afin qu'il corresponde à vos besoins. Cela peut-être fait à l'aide du menu
  \ <menu|Edit|Preferences>. Le plus important est de choisir l'aspect et le
  comportement dans <menu|Edit|Preferences|Look and feel> . Cela vous permet,
  par exemple, de definir des raccourcis clavier de <TeXmacs> identiques à
  ceux utilisés dans d'autres applications.

  Les personnalisations suivantes sont disponibles :

  <\description>
    <item*|<menu|Look and feel>><label|preferences:look-and-feel>Ce choix
    controle l'aspect et le comportement général de <TeXmacs>, et en grande
    partie le comportement du clavier. La <menu|default> dépend de votre
    système (<menu|Gnome>, <menu|KDE> ou <menu|Emacs> sous <name|Linux>,
    <menu|Mac OS> sous <name|Mac OS>, and <menu|Windows> sous
    <name|Windows>). L'aspect <menu|Emacs> peut-être utilisé comme
    alternative sous tous les systèmes; Cela a été le défaut pour toutes les
    versions de <TeXmacs> avant la 1.0.7.6.

    Plus de details sur \ <hlink|keyboard configuration on different
    systems|man-config-keyboard.en.tm> peuvent être trouvés ci-dessous.

    <item*|<menu|Interactive questions>>Ce choix determine comment
    l'utilisateur sera informé qu'une entrée est attendue. Cela peut-être
    réalisé dans une fenêtre séparée ou dans la barre de status de
    \ <TeXmacs>.

    <item*|<menu|Details in menus>>Ce choix determine le niveau de detail des
    menus. Les choix les moins souvent utilisés seront écartés par la
    sélection de <menu|Simplified menus>.

    <item*|<menu|View>>Ce choix est le même que celui du menu racine.

    <item*|<menu|Language>>Votre langue préférée pour l'interface <TeXmacs> .

    <item*|<menu|Keyboard>><label|preferences:keyboard>En plus de l'aspect
    général, quelques selecteurs \ déterminent le comportement du clavier:

    <\itemize>
      <item>Le <menu|Cyrillic input method> determine <hlink|how to type text
      in Cyrillic languages|man-russian.en.tm>.

      <item>Les guillements peuvent être automatiquement fermés à l'aide du
      style <menu|Automatic quotes> .

      <item>Les paranthèses peuvent être automatiquement fermées à l'aide de
      : \ <menu|Automatically close brackets>.
    </itemize>

    <item*|<menu|Printer>>L'imprimante peut être configurée à l'aide de ce
    sous-menu.

    <item*|<menu|Security>>En théorie, les documents <TeXmacs> peuvent
    inclure des macros ou des hyperlinks qui permettent l'execution de
    commandes arbitraires (définies par l'auteur). En pratique, cette
    fonctionnalité peut produire un trou de sécurité. C'est pourquoi la
    péférence <menu|Security> permet à l'utilisateur de définir ce qui
    peut-être fait par un code executable non certifié.

    <item*|<menu|Converters>>Le comportement des convertisseurs entre
    \ <TeXmacs> et les divers autres formats peut être configuré à partir de
    ce menu. Pour plus de détails, vous référez au <hlink|chapter on
    compatibility with other formats|../convert/man-convert.en.tm>.

    <item*|<menu|Scripts>>Définit le language de script par défaut pour les
    scripts externes.

    <item*|<menu|Tools>><TeXmacs> offre quelques outils additionnels à
    destination de l'utilisateur pour des usages particuliers :

    <\itemize>
      <item>Un outil de debogage pour les développeurs <TeXmacs> .

      <item>Un créateur de liens pour entrer des hyperliens et des
      annotations complexes.

      <item>Un outil de gestion des versions pour comparer deux versions d'un
      document <TeXmacs> .

      <item>Une connection a distance (qui ne fonctionne plus en ce moment).
    </itemize>

    <item*|<menu|Autosave>>Ce choix determine la fréquence de la sauvegarde
    automatique. Toute edition d'un fichier qui n'est pas en sauvegarde
    automatique, sera perdue en cas de terminaison anormale de <TeXmacs>.
    Cela se produit typiquement aprés une fausse manoeuvre de la part de
    l'utilisateur, à cause de certains bugs dans <TeXmacs>, ou à cause d'une
    coupure de courant.

    <item*|><item*|<menu|Bibtex command>>L'utilisateur peut définier une
    alternative à \ <verbatim|bibtex> pour la compilation des bibliographies
    nécessitant <BibTeX>. Notez que les versionns récentes de <TeXmacs>
    incluent un outil alternatif de compilation des bibliographies.
  </description>

  <tmdoc-copyright|1998--2013|Joris van der Hoeven, Denis Raux>

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