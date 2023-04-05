<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Probl�mes de mise en page>

  En g�n�ral, <TeXmacs> se charge de la mise en page du texte. Par
  cons�quent, nous vous recommandons de ne pas la faire vous-m�me, bien que
  cela ne soit pas impossible. Par exemple, vous ne devez pas ins�rer
  d'espaces ou de lignes vierges suppl�mentaires entre les mots ou les
  lignes. Ces espaces verticaux ou horizontaux doivent �tre ins�r�s
  explicitement avec <menu|Ins�rer|Espace>. Cela vous permettra de g�rer
  votre document de mani�re plus souple lors de changements mineurs affectant
  les sauts de page ou de ligne, ou de changements majeurs comme la
  modification du style du document.

  On a impl�ment� diff�rents types d'espaces explicites. Tout d'abord, on
  peut ins�rer des espaces fixes ; leur largeur et leur hauteur sont fixes.
  Les espaces horizontaux ont une hauteur nulle et sont soit �tirables, soit
  non �tirables. La longueur des espaces �tirables d�pend de la c�sure du
  paragraphe. De plus, il est possible d'ins�rer des tabulations. Les espaces
  verticaux peuvent �tre ins�r�s au d�but ou � la fin d'un paragraphe.
  L'espace vertical r�el entre deux paragraphes correspond au maximum entre
  l'espace vertical apr�s le premier paragraphe et l'espace vertical avant le
  second (ceci permet d'�viter un espace disproportionn� entre deux
  th�or�mes, contrairement � \ <TeX>).

  En ce qui concerne le paragraphe, l'utilisateur peut indiquer le style du
  paragraphe (justifi�, cadr� � gauche, centr�, cadr� � droite), les marges
  et l'indentation � gauche (resp. � droite) de la premi�re (resp. derni�re)
  ligne. On peut aussi contr�ler l'espace entre paragraphes et lignes d'un
  m�me paragraphe.

  Vous pouvez indiquer la mise en page avec <menu|Document|Page>. Tout
  d'abord, choisissez la fa�on dont les pages sont affich�es sur l'�cran ; si
  vous choisissez \Spapier\T comme type de page dans
  <menu|Document|Page|Type>, les sauts de page seront visibles. Par d�faut,
  le type de page est \Spapyrus\T, ce qui �vite de voir les sauts de page
  lors de la cr�ation du document. Le type de page \Sautomatique\T correspond
  � une taille de papier identique � la taille de la fen�tre. Les marges de
  la page et la largeur du texte sont sp�cifi�s avec <menu|Document|Page|Mise
  en page>. Il est souvent pratique de r�duire les marges de la page
  lorsqu'on la visualise ; on peut le faire avec
  <menu|Document|Page|Apparence � l'�cran>.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Mich�le Garoche, Daouda
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