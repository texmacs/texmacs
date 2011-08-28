<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Annuler et rétablir>

  Il est possible d'annuler un par un les changements faits dans un document
  à partir du moment où vous avez lancé <apply|TeXmacs>. Pour ce faire,
  utilisez <apply|menu|Edit|Undo> ou utilisez les combinaisons
  <shortcut|(undo 0)> ou <shortcut|(undo 0)>. Pour rétablir les changements
  <space|0.2spc>annulés<space|0.2spc>, utilisez <apply|menu|Edit|Redo> ou
  <shortcut|(redo 0)>.

  Pour ne pas utiliser trop de mémoire, le nombre d'actions successives
  annulables est limité à 100 (par défaut). Vous pouvez changer ce nombre en
  insérant la commande suivante :

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  dans votre fichier d'initialisation personnel (voir
  <apply|menu|Help|Manual|Customizing TeXmacs>). Si vous spécifiez un nombre
  négatif, vous pourrez alors annuler un nombre illimité d'actions.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Annuler>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Répéter>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Aide>|<with|font
      family|<quote|ss>|Scheme>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
