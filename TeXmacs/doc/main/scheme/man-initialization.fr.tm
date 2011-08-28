<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Personnalisation des fichiers d'initialisation>

  Lors de son lancement, <apply|TeXmacs> exécute le fichier
  <verbatim|$TEXMACS_PATH/progs/init-texmacs.scm>, sauf si vous créez votre
  propre fichier d'initialisation dans <verbatim|$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm>.
  Par défaut, le chemin <verbatim|$TEXMACS_HOME_PATH> correspond à
  <verbatim|.TeXmacs>. Vous voudrez peut-être ajouter certaines actions au
  fichier d'initialisation par défaut. Dans ce cas, n'oubliez pas d'inclure
  la commande suivante :\ 

  <\verbatim>
    \ \ \ \ (exec-file "$TEXMACS_PATH/progs" "init-texmacs.scm")
  </verbatim>

  dans votre fichier d'initialisation. De même, le fichier
  <verbatim|$TEXMACS_PATH/progs/init-buffer.scm> est exécuté chaque fois que
  vous créez un nouveau tampon, sauf si vous créez votre propre fichier
  d'initialisation <verbatim|$TEXMACS_HOME_PATH/progs/my-init-buffer.scm>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Michèle Garoche>

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
