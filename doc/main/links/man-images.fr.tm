<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Insertion d'images>

  Vous pouvez inclure des images dans le document avec
  <apply|menu|Insert|Image>. Actuellement, <apply|TeXmacs> reconnaît les
  formats de fichier <verbatim|ps>, <verbatim|eps>, <verbatim|tif>,
  <verbatim|pdf>, <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>,
  <verbatim|xpm> et <verbatim|fig>. Ici, <verbatim|gs> (c'est-à-dire
  ghostscript) est utilisé pour afficher les images PostScript. Si
  ghostscript n'est pas installé sur votre système, vous pouvez télécharger
  le logiciel à partir de :

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  À l'heure actuelle, les autres formats de fichier sont convertis en
  fichiers PostScript avec les scripts <verbatim|tiff2ps>, <verbatim|pdf2ps>,
  <verbatim|pnmtops>, <verbatim|giftopnm>, <verbatim|ppmtogif> et
  <verbatim|xpmtoppm>. Si ces scripts ne sont pas installés sur votre
  système, contactez votre administrateur système.

  Par défaut, les images sont affichées à leur taille d'origine. Les
  opérations suivantes sont possibles sur les images :\ 

  <\itemize>
    <item>Troncature d'une image. C'est le coin inférieur gauche de l'image
    qui est pris comme origine du rectangle de troncature.

    <item>Recadrage d'une image. Si vous spécifiez une nouvelle hauteur, mais
    aucune largeur (ou vice-versa), l'image est recadrée proportionnellement
    par rapport à son origine.

    <item>Agrandissement proportionnel d'une image. Une autre façon
    d'agrandir l'image en appliquant un facteur constant à la hauteur et la
    largeur.
  </itemize>

  On a aussi inclus un script de conversion des images contenant des formules
  <apply|LaTeX> en fichier encapsulé PostScript. Pour inclure une formule
  <apply|LaTeX> dans une image <verbatim|xfig>, vous devez saisir la formule
  en tant que texte, sélectionner une police <apply|LaTeX> et insérer le
  drapeau spécial dans le texte.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Image>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
