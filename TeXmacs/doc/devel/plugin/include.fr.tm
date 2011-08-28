<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Intégration de votre système dans <apply|TeXmacs>>

  Supposons que vous avez réussi à écrire votre première interface avec
  <apply|TeXmacs> à l'aide des explications de la section précédente. Il est
  temps maintenant d'inclure la gestion de votre système dans la distribution
  standard de <apply|TeXmacs> distribution, après quoi vous pourrez
  l'améliorer.

  Depuis la sortie de la version 1.0.1.5, il est devenu très facile d'adapter
  une interface de façon à ce qu'elle puisse être directement intégrée dans
  <TeXmacs>. Il suffit de créer un répertoire :

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/myplugin
  </verbatim>

  où <verbatim|myplugin> est le nom de votre plugin. Nous vous rappelons que
  <verbatim|$TEXMACS_HOME_PATH> est assimilé à <verbatim|~/.TeXmacs> par
  défaut. Vous trouverez dans le répertoire <verbatim|$TEXMACS_PATH/plugins>
  tous les plugins standards qui sont livrés avec <TeXmacs>. Servez-vous en
  de base pour construire les vôtres.

  Le répertoire <verbatim|myplugin> devra contenir une structure de
  répertoire similaire à la structure du répertoire <verbatim|$TEXMACS_PATH>,
  quoique vous puissiez omettre les sous-répertoires dont vous ne vous servez
  pas. Néanmoins, il vous faudra créer un fichier
  <verbatim|progs/init-myplugin.scm> qui décrira l'initialisation de votre
  plugin. En général, ce fichier contient juste une instruction <name|Scheme>
  de la forme suivante :

  <\verbatim>
    \ \ \ \ (plugin-configure myplugin<format|next line> \ \ \ \ \ (:require
    (file-in-path "myplugin"))<format|next line> \ \ \ \ \ (:launch
    "shell-cmd")<format|next line> \ \ \ \ \ (:format "input-format"
    "output-format")<format|next line> \ \ \ \ \ (:session "Myplugin"))
  </verbatim>

  La première instruction est un prédicat qui teste si votre plugin peut être
  utilisé sur un système donné. En général, il vérifie qu'un programme donné
  est accessible via votre PATH. Les autres instructions ne sont exécutées
  que si ce premier point est vérifié. L'instruction <verbatim|:launch>
  spécifie que votre plugin sera lancé avec <verbatim|shell-cmd>. La commande
  <verbatim|shell-cmd> est généralement de la forme <verbatim|myplugin
  --texmacs>. L'instruction <verbatim|:format> spécifie les formats d'entrée
  et de sortie à utiliser. En général, <verbatim|input-format> correspond à
  <verbatim|verbatim> et <verbatim|output-format> à <verbatim|generic>. Les
  autres formats possibles sont : <verbatim|scheme>, <verbatim|latex>,
  <verbatim|html> et <verbatim|ps>. L'instruction <verbatim|:session> rend
  les sessions shell disponible pour votre plugin à partir du menu
  <apply|menu|Insert|Session|Myplugin>.

  Si tout fonctionne correctement et que vous souhaitez faire profiter les
  autres de votre système dans la version officielle de <apply|TeXmacs>
  distribution, contactez-moi à <verbatim|vdhoeven@texmacs.org>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Myplugin>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
