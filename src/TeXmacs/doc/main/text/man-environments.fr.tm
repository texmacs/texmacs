<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Environnements>

  De la même façon que les marqueurs de contenu, les environnements servent à
  délimiter des parties de texte ayant une signification particulière.
  Néanmoins, les environnements englobent, en général, des parties de texte
  comportant plusieurs paragraphes, tandis que les <apply|hyper-link|balises
  de contenu|man-content-tags.fr.tm> englobent des portions plus petites. Les
  environnements les plus souvent utilisés en mathématiques sont
  <markup|theorem> et <markup|proof>, comme dans l'exemple ci-dessous :

  <\theorem>
    Il n'existe pas de nombres entiers positifs a, b, c, n avec
    \ <with|mode|math|n\<geqslant\>3>, tels que
    <with|mode|math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    Je n'ai pas la place de faire la démonstration ici.
  </proof>

  Vous pouvez activer un environnement avec <apply|menu|Text|Environment>.
  D'autres environnements, tels <markup|proposition>, <markup|lemma>,
  <markup|corollary>, <markup|axiom>, <markup|definition>, génèrent un rendu
  similaire à celui de théorème. Utilisez la macro <markup|dueto> (avec
  <key|\\ d u e t o retour chariot>) pour indiquer le nom de la ou des
  personne(s) à qui le théorème est dû :\ 

  <\theorem>
    <dueto|Pythagore>Dans certains cas, on a :
    <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  D'autres environnements fréquemment utilisés génèrent un rendu similaire à
  celui de théorème, mais ne mettent pas le texte en relief. Ce sont
  <markup|remark>, <markup|note>, <markup|example>, <markup|warning>,
  <markup|exercise> et <markup|problem>. Les autres environnements :
  <markup|verbatim>, <markup|code>, <markup|quote>, <markup|quotation> et
  <markup|verse> sont utilisés pour saisir du texte sur plusieurs paragraphes
  ou du code, des citations ou des poésies.

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
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-20|<tuple|2|?>>
    <associate|idx-10|<tuple|2|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|2|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|2|?>>
    <associate|idx-15|<tuple|2|?>>
    <associate|idx-16|<tuple|2|?>>
    <associate|idx-17|<tuple|2|?>>
    <associate|idx-18|<tuple|2|?>>
    <associate|idx-19|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Environnement>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proposition>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|lemma>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corollary>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|axiom>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|definition>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|remark>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|note>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|example>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|warning>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercise>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|problem>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quote>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quotation>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verse>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
