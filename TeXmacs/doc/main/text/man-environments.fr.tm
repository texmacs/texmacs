<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Environnements>

  De la même façon que les marqueurs de contenu, les environnements servent à
  délimiter des parties de texte ayant une signification particulière.
  Néanmoins, les environnements englobent, en général, des parties de texte
  comportant plusieurs paragraphes, tandis que les <hlink|balises de
  contenu|man-content-tags.fr.tm> englobent des portions plus petites. Les
  environnements les plus souvent utilisés en mathématiques sont
  <markup|theorem> et <markup|proof>, comme dans l'exemple ci-dessous :

  <\theorem>
    Il n'existe pas de nombres entiers positifs a, b, c, n avec
    \ <math|n\<geqslant\>3>, tels que <math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    Je n'ai pas la place de faire la démonstration ici.
  </proof>

  Vous pouvez activer un environnement avec <menu|Insert|Environment>.
  D'autres environnements, tels <markup|proposition>, <markup|lemma>,
  <markup|corollary>, <markup|axiom>, <markup|definition>, génèrent un rendu
  similaire à celui de théorème. Utilisez la macro <markup|dueto> (avec
  <key|\\ d u e t o retour chariot>) pour indiquer le nom de la ou des
  personne(s) à qui le théorème est dû :\ 

  <\theorem>
    <dueto|Pythagore>Dans certains cas, on a :
    <math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  D'autres environnements fréquemment utilisés génèrent un rendu similaire à
  celui de théorème, mais ne mettent pas le texte en relief. Ce sont
  <markup|remark>, <markup|note>, <markup|example>, <markup|warning>,
  <markup|exercise> et <markup|problem>. Les autres environnements :
  <markup|verbatim>, <markup|code>, <markup|quote>, <markup|quotation> et
  <markup|verse> sont utilisés pour saisir du texte sur plusieurs paragraphes
  ou du code, des citations ou des poésies.

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