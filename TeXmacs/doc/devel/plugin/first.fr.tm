<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Création de votre première interface avec
  <apply|TeXmacs>>

  Pour créer votre première interface avec <apply|TeXmacs>, nous vous
  recommandons de suivre les étapes suivantes :

  <\enumerate>
    <item>Créez une option <verbatim|--texmacs> dans votre programme. Elle
    sera utilisée pour appeler votre programme dans <apply|TeXmacs>.

    <item>Modifiez vos routines de sorties de telle manière que les sorties
    soient envoyées à <apply|TeXmacs> lorsque votre programme est lancée avec
    l'option <verbatim|--texmacs>.

    <item>Créez un script <verbatim|mycas> dans votre PATH qui lancera votre
    programme avec l'option <verbatim|--texmacs>.
  </enumerate>

  Après avoir fait cela, votre programme sera accessible sous le nom de
  <apply|menu|Mycas> via <apply|menu|Insert|Session>. Nous vous expliquerons
  plus loin comment afficher votre système sous son vrai nom, comment le
  personnaliser et comment intégrer l'interface dans la distribution
  officielle de <apply|TeXmacs>.

  En général, l'étape 2 est la plus compliquée et le temps passé dessus
  dépend de l'architecture de votre système. Si vous créez des routines de
  sortie bien pensées (y compris les routines pour afficher les messages
  d'erreur), il vous suffira de les modifier conformément à l'exemple
  <verbatim|mycas> et de réutiliser des routines de sortie <apply|LaTeX>
  fournies par la plupart des systèmes.

  Actuellement, <apply|LaTeX> n'est implémenté que comme format de
  transmission standard de formules mathématiques, car c'est le format le
  plus utilisé. Nous avons prévu d'implémenter plus tard des formats plus
  sûrs du point de vue sémantique. Ne perdez pas de vue non plus que vous
  pouvez envoyer vos sorties sous forme d'arbre.

  Néanmoins, nous avons enrichi le format <apply|LaTeX> standard des
  commandes <verbatim|\\*> et <verbatim|\\bignone> qui servent à la
  multiplication et à la fermeture des grands opérateurs. Ceci permet de
  faire la distinction entre :

  <\verbatim>
    \ \ \ \ a \\* (b + c)
  </verbatim>

  (ou <with|mode|math|a> multiplié par <with|mode|math|b+c>) et :\ 

  <\verbatim>
    \ \ \ \ f(x + y)
  </verbatim>

  (ou <with|mode|math|f> appliqué à <with|mode|math|x+y>). De même, dans :\ 

  <\verbatim>
    \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
  </verbatim>

  la commande <verbatim|\\bignone> est utilisée pour spécifier le domaine
  d'application des opérateurs <verbatim|\\sum>.

  Il s'avère que l'utilisation systématique des commandes <verbatim|\\*> et
  <verbatim|\\bignone>, ainsi que de sorties <apply|LaTeX> correctes pour les
  autres constructions, permet <with|font shape|italic|a priori> de donner
  une signification claire à votre sortie. Par exemple, on peut ainsi écrire
  des routines supplémentaires pour couper et coller des formules entre
  systèmes différents.

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
    <associate|idx-1|<tuple|3.|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|3.|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
