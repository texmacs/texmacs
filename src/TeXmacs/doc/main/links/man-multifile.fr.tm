<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Livres et documents maîtres>

  Quand un document devient très volumineux, vous pouvez le subdiviser en
  plusieurs documents plus petits. Ceci permet de réutiliser plus facilement
  les composants du document maître dans d'autres ouvrages et améliore les
  temps de réaction de l'éditeur. On peut insérer un fichier dans un autre
  avec <apply|menu|Insert|Link|Include>. Les documents inclus sont mis en
  tampon pour accélérer leur traitement. Utilisez
  <apply|menu|Tools|Update|Inclusions> pour mettre à jour tous les documents
  inclus.

  Quand on écrit un livre, on crée, en général, autant de fichiers
  <verbatim|c1.tm>, <verbatim|c2.tm> ..., <verbatim|cn.tm> que de chapitres.
  On crée, ensuite, un fichier pour le livre <verbatim|book.tm>, dans lequel
  on insère les fichiers <verbatim|c1.tm>, <verbatim|c2.tm>, ...,
  <verbatim|cn.tm> en utilisant le mécanisme décrit ci-dessus. La table des
  matières, la bibliographie, etc... sont, en général, générées dans le
  fichier maître <verbatim|book.tm>.

  Pour afficher les références croisées à d'autres chapitres lorsqu'on édite
  un fichier <verbatim|ci.tm>, on peut définir <verbatim|book.tm> en tant que
  <space|0.2spc>fichier maître<space|0.2spc> des fichiers <verbatim|c1.tm>,
  ..., <verbatim|cn.tm> avec <apply|menu|Document|Master|Attach>. À l'heure
  actuelle, les numéros de chapitre ne sont pas gérés par ce mécanisme. Vous
  devez définir la variable d'environnement <verbatim|chapternr> au début de
  chaque chapitre pour qu'il soit numéroté correctement lors de son édition.

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Inclure>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Outils>|<with|font
      family|<quote|ss>|Actualiser>|<with|font
      family|<quote|ss>|Inclusions>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Maître>|<with|font
      family|<quote|ss>|Attacher>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
