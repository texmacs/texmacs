<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formules mathématiques>

  Pour saisir des formules mathématiques, vous devez d'abord passer en
  <space|0.2spc>math mode<space|0.2spc> en appuyant sur la touche <key|$>
  ou en insérant une équation (avec <apply|menu|Insert|Mathematics|Equation>).
  En mode maths, il existe des commandes spécifiques et des raccourcis pour
  saisir des symboles et des formules mathématiques. Par exemple, le préfixe
  <prefix|M-A-> permet de saisir des symboles grecs (rappelez-vous que <prefix|M-A->
  est équivalent à <prefix|math:greek>, <key|escape escape escape> ou <prefix|A-C->).

  L'éditeur traduit les formules selon certaines règles. Cette
  caractéristique, qui sera développée dans les versions suivantes, est utile
  lors de la communication avec un logiciel de calcul formel. Pour l'instant,
  vous devez saisir explicitement le signe multiplié <key|*> entre deux
  symboles <with|mode|math|a> et <with|mode|math|b>. Par défaut, la saisie de
  <key|a b> donne <with|mode|math|mode|text|ab> et non <with|mode|math|a*b>.

  <\traverse>
    <apply|branch|Principaux objets mathématiques|keyboard/man-main.fr.tm>

    <apply|branch|Symboles mathématiques|keyboard/man-symbols.fr.tm>

    <apply|branch|Grands symboles|keyboard/man-big.fr.tm>

    <apply|branch|Grands délimiteurs|keyboard/man-large.fr.tm>

    <apply|branch|Larges accents|keyboard/man-wide.fr.tm>
  </traverse>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insérer>|<with|font
      family|<quote|ss>|Mathématiques>|<with|font
      family|<quote|ss>|Équation>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
