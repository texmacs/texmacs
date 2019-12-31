<TeXmacs|1.99.12>

<style|<tuple|tmdoc|french|old-spacing|old-dots>>

<\body>
  <tmdoc-title|Impression de documents>

  Vous pouvez imprimer le fichier actif avec <menu|File|Print|Print all>.
  L'imprimante définie par défaut dans <TeXmacs> a une résolution de 600 dpi
  et gère un format de papier A4. Vous pouvez changer ces réglages avec
  <menu|File|Page setup...> . Vous pouvez aussi générer un fichier PostScript
  avec <menu|File|Print|Print all to file> (dans ce cas, ce sont les réglages
  de l'imprimante par défaut qui sont utilisés pour créer le fichier
  résultant) ou <menu|File|Export|Postscript> (dans ce cas, les réglages de
  l'imprimante ne sont pas pris en compte).

  Vous pouvez générer un ficier PDF avec <menu|File|Export|Pdf>. Notez que
  vous devez définir <menu|Éditer|Préférences|Imprimante|Type de police|Type
  1> si vous souhaitez que le Postcript ou PDF produit soit de police de
  <name|Type 1.> Cependant, seules les polices CM peuvent recevoir la version
  <name|Type 1>. Ces polices CM sont d'une qualité légèrement inférieure aux
  polices EC principalement pour les caractères accentués. Par conséquent,
  vous pourriez préférer utiliser les polices CE aussi longtemps que vous
  n'avez pas besoin d'un fichier PDF qui soit joli dans <name|Acrobat
  Reader>.

  Quand <TeXmacs> est configuré correctement, l'éditeur est un vrai éditeur
  <em|tel écran, tel écrit :> le résultat à l'impression est identique à ce
  que vous voyez sur l'écran. Pour obtenir ce résultat, vous devez
  sélectionner <menu|Document|Page|Type|Paper> et <menu|Document|Page|Screen
  layout|Marges comme sur le papier>. Vous devez aussi vous assurer que les
  caractères sur l'écran utilisent le même nombre de points par pouce que
  votre imprimante. La résolution peut être modifiée avec
  <menu|Document|Font|Dpi>. À l'heure actuelle, il est possible que le
  changement de dpi entraîne des changements typographiques mineurs qui se
  répercutent sur l'ensemble du document et affectent les sauts de ligne et
  de page. Ce problème sera résolu dans une prochaine version.

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
    <associate|page-medium|paper>
  </collection>
</initial>