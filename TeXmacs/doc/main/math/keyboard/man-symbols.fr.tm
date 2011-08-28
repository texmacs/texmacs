<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Saisie de symboles mathématiques>

  Dans <apply|TeXmacs>, on accède aux caractères grecs en combinant la touche
  <prefix|M-A-> avec une lettre. Par exemple, <key|H-a> produit le caractère
  <with|mode|math|\<alpha\>> et <key|H-G> le caractère
  <with|mode|math|\<Gamma\>>. <apply|hyper-link|Souvenez-vous|../../start/man-conventions.fr.tm>
  que la touche <prefix|math:greek> est équivalente à <prefix|M-A-> ; vous pouvez donc aussi
  utiliser <key|F5 r> pour obtenir <with|mode|math|\<rho\>>. De même,
  <prefix|math:bold>, <prefix|math:cal>, <prefix|math:frak> et <prefix|math:bbb> permettent d'obtenir des
  caractères gras, calligraphiques, gothiques ou des onciales. Par exemple,
  <shortcut|\<frak-m\>> donne <with|mode|math|\<frak-m\>>, <key|S-F6 R> donne <format|no
  line break><with|mode|math|\<bbb-R\>> et <shortcut|\<b-cal-Z\>> donne
  <with|mode|math|\<b-cal-Z\>>.

  Les caractères grecs peuvent aussi être obtenus en tant que
  <space|0.2spc>variantes<space|0.2spc> de caractères latins en utilisant
  la touche <key|tab>. Par exemple, <key|p tab> donne
  <with|mode|math|\<pi\>>. La touche <key|tab> sert aussi à produire des
  variantes de lettres grecques. Par exemple, <key|H-p tab tab> et <key|p tab
  tab tab> donnent toutes les deux <with|mode|math|\<varpi\>>.

  On peut obtenir de nombreux autres symboles mathématiques par des
  combinaisons de touches <space|0.2spc>évidentes<space|0.2spc>. Par
  exemple, \ <key|- \<gtr\>> donne <with|mode|math|\<rightarrow\>>, <key|- -
  \<gtr\>> donne <with|mode|math|\<longrightarrow\>> et <key|\<gtr\> => donne
  <with|mode|math|\<geqslant\>>. De même, <key|\| -> donne
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> donne
  <with|mode|math|\<mapsto\>> et <key|- \<gtr\> \<less\> -> donne
  <with|mode|math|\<rightleftarrows\>>. Voici quelques règles générales
  permettant d'obtenir des variantes de symboles :

  <\description>
    <expand|item*|<key|tab>>est la touche principale pour obtenir des
    variantes. Par exemple, <key|\<gtr\> => donne
    <with|mode|math|\<geqslant\>>, mais <key|\<gtr\> = tab> donne <format|no
    line break><with|mode|math|\<geq\>>. De même, <key|\<less\> tab> donne
    <with|mode|math|\<prec\>>, <key|\<less\> tab => donne
    <with|mode|math|\<preccurlyeq\>> et <key|\<less\> tab = tab> donne
    <with|mode|math|\<preceq\>>. <key|P tab tab> donne
    <with|mode|math|\<wp\>> et <key|e tab tab> donne la constante d'Euler. On
    peut utiliser <key|S-tab> pour passer en revue les différents caractères
    produits par les touches <key|tab> successives.

    <expand|item*|<key|@>>(symbole @) est utilisée pour inscrire un symbole
    dans un cercle ou un carré. Par exemple, <key|@ +> donne
    <with|mode|math|\<oplus\>> et <key|@ x> donne
    <with|mode|math|\<otimes\>>. De même, <key|@ tab +> donne
    <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>est utilisée pour les négations. Par exemple,
    <key|= /> donne <with|mode|math|\<neq\>> et <key|\<less\> = /> donne
    <with|mode|math|<neg|\<leqslant\>>>. Notez que <key|\<less\> = tab tab />
    donne <with|mode|math|\<nleqq\>>, tandis que <key|\<less\> = tab tab /
    tab> donne <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>est utilisée après une flèche pour forcer le
    caractère suivant à s'inscrire au-dessus ou au-dessous d'elle. Par
    exemple, <key|- - \<gtr\> ^ x> donne <with|mode|math|\<longrightarrow\><rsup|x>
    >, mais <key|- - \<gtr\> ! ^ x> donne
    <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Certains symboles ne peuvent être obtenus comme ci-dessus, il faut alors
  utiliser le préfixe <prefix|symbol>. En voici le tableau :

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Raccourcis>|<cell|Symboles>|<cell|Raccourcis>|<cell|Symboles>>|<row|<cell|<key|symbol a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<key|symbol n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<key|symbol u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<key|symbol v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<key|symbol w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Symboles
  ne pouvant être obtenus par les règles générales édictées ci-dessus.>

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
    <associate|gly-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Symboles ne pouvant être obtenus par les règles générales
      édictées ci-dessus.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
