<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Création d'étiquettes, de liens et de références>

  Vous pouvez créer une nouvelle étiquette inactive avec \ <shortcut|(make-label)>
  ou <apply|menu|Insert|Link|Label> et une référence à cette étiquette avec
  <shortcut|(make 'reference)> ou <apply|menu|Insert|Link|Reference>. Faites attention
  à mettre l'étiquette à un endroit où sa numérotation est correcte. Lors de
  l'étiquetage de sections, il vaut mieux placer l'étiquette juste après le
  nom de la section. Lors de l'étiquetage d'équations, il vaut mieux placer
  l'étiquette juste au début de l'équation.

  On peut créer des hyperliens vers d'autres documents avec
  <key|inactive \<gtr\>> ou <apply|menu|Insert|Link|Hyperlink>. Le premier
  champ de l'hyperlien est le texte associé, affiché en bleu lorsque le lien
  est activé. Le deuxième champ contient le nom d'un document, qui peut être
  sur la toile. Comme d'habitude pour les hyperliens, un lien de la forme
  <verbatim|#<with|font shape|italic|étiquette>> pointe vers une étiquette à
  l'intérieur du document et un lien de la forme <verbatim|<with|font
  shape|italic|url>#<with|font shape|italic|étiquette>> pointe vers une
  étiquette dans le document situé à l'adresse <verbatim|<with|font
  shape|italic|url>>.

  De même, on peut associer une action à un texte ou une image avec
  <key|inactive *> ou <apply|menu|Insert|Link|Action>. Le second champ
  contient alors un script Guile/Scheme, qui est exécuté quand on
  double-clique sur le texte après son activation. Pour des raisons de
  sécurité, ces scripts sont parfois refusés. Par défaut, on vous demande si
  vous acceptez le script ; on peut changer ce comportement par défaut avec
  <apply|menu|Edit|Preferences|Security>. Notez que la commande Guile/Scheme
  :\ 

  <\verbatim>
    \ \ \ \ (system "commande-shell")
  </verbatim>

  évalue <verbatim|commande-shell> en tant que commande shell.

  Enfin, vous pouvez inclure directement d'autres documents dans un document
  donné avec <key|inactive i> ou <apply|menu|Insert|Link|Include>. Cela vous
  permet, par exemple, d'inclure le listing d'un programme dans votre
  document de telle façon que les modifications dans votre programme se
  reflètent automatiquement dans le document.

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
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Étiquette>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Référence>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Hyperlien>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Action>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Options>|<with|font
      family|<quote|ss>|Securité>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Inclure>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
