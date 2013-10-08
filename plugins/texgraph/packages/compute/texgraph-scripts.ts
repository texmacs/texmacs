<TeXmacs|1.0.7.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|texgraph-scripts|1.0>

    <\src-purpose>
      Complements to scripts-1.0 for TeXgraph
    </src-purpose>

    <\src-copyright|2009>
      Emmanuel Corcelle

      Based on scripts.ts by Joris van der Hoeven
    </src-copyright>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|scripts>

  <\active*>
    <\src-comment>
      Plots.
    </src-comment>
  </active*>

  <assign|texgraph-plot-settings|<macro|settings|fun-f|<style-with|src-compact|none|<resize|
  |||3fn|><resize|<small|<with|font-shape|italic|réglages pour <arg|fun-f>>>
  : |||7fn|><plot-input-field|0.47par|<verbatim|<arg|settings>>>>>>

  <assign|texgraph-plot-choices-3|<macro|title1|choice1|title2|choice2|title3|choice3|<style-with|src-compact|none|<resize|<arg|title1>:
  |||8fn|><plot-input-field|0.075par|<arg|choice1>><resize| <arg|title2>:
  |||7fn|><plot-input-field|0.075par|<math|<arg|choice2>>><resize|
  <arg|title3>: |||7fn|><plot-input-field|0.075par|<math|<arg|choice3>>>>>>

  <assign|texgraph-plot-choices-2|<macro|title4|choice4|title5|choice5|<style-with|src-compact|none|<resize|<arg|title4>:
  |||8fn|><plot-input-field|0.075par|<arg|choice4>><resize| <arg|title5>:
  |||7fn|><plot-input-field|0.3par|<verbatim|<arg|choice5>>>>>>

  <assign|texgraph-plot-commands|<\macro|commands>
    <plot-input-field|0.65par|<verbatim|<arg|commands>>>
  </macro>>

  <assign|texgraph-plot-infos|<macro|text|<style-with|src-compact|none|<resize|
  |||3fn|><style-with|src-compact|none|<resize|<small|<arg|text>>||<minus|1b|0.3fn>||<plus|1t|0.3fn>>>>>>

  <assign|texgraph-plot-curve|<macro|fun-f|settings-f|fun-g|settings-g|fun-h|settings-h|axes|grad-x|grad-y|origin|set-axes|grid|set-grid|size|scale-x|scale-y|start-x|end-x|start-y|end-y|commands|<\plot|<localize|Courbes
  TeXgraph>>
    <no-indent><plot-group|<localize|Fonctions>>

    <no-indent><plot-function|f(x)=|<arg|fun-f>>

    <no-indent><texgraph-plot-settings|<arg|settings-f>|<math|f>>

    <no-indent><plot-function|g(x)=|<arg|fun-g>>

    <no-indent><texgraph-plot-settings|<arg|settings-g>|<math|g>>

    <no-indent><plot-function|h(x)=|<arg|fun-h>>

    <no-indent><texgraph-plot-settings|<arg|settings-h>|<math|h>>

    \;

    <no-indent><plot-group|<localize|Axes>>

    <no-indent><texgraph-plot-choices-3|<small|Tracés
    (oui/non)>|<arg|axes>|<small|<math|Grad x
    (unit)>>|<arg|grad-x>|<small|<math|Grad y (unit)>>|<arg|grad-y>>

    <no-indent><texgraph-plot-choices-2|<small|Origine du
    repère>|<arg|origin>|<small|<with|font-shape|italic|réglages
    axes>>|<arg|set-axes>>

    \;

    <no-indent><plot-group|<localize|Grille>>

    <no-indent><texgraph-plot-choices-2|<small|Tracée
    (oui/non)>|<arg|grid>|<small|<with|font-shape|italic|réglages
    grille>>|<arg|set-grid>>

    \;

    <no-indent><plot-group|<localize|Fenetre>>

    <no-indent><texgraph-plot-choices-3|<small|Largeur
    (+i*haut.)>|<arg|size>|<small|ou échelle / <math|O
    x>>|<arg|scale-x>|<small|et échelle / <math|O y>>|<arg|scale-y>>

    <no-indent><plot-range|x|<arg|start-x>|<arg|end-x>>

    <no-indent><plot-range|y|<arg|start-y>|<arg|end-y>>

    \;

    <no-indent><plot-group|<localize|Commandes complémentaires>>

    <no-indent><texgraph-plot-commands|<arg|commands>>

    <no-indent><texgraph-plot-infos|Pour les réglages, menu :
    <samp|TeXgraph<math|\<rightarrow\>>Proprietes des lignes>>

    <no-indent><texgraph-plot-infos|par exemple : <verbatim|Color:=blue,
    LineStyle:=dashed,>>
  </plot>>>

  <assign|texgraph-plot-curve*|<macro|fun-x|fun-y|start-t|end-t|<\plot|<localize|Plot
  parametric curve>>
    <plot-group|<localize|Function>>

    <plot-function|x|<arg|fun-x>>

    <plot-function|y|<arg|fun-y>>

    <plot-group|<localize|Range>>

    <plot-range|t|<arg|start-t>|<arg|end-t>>
  </plot>>>

  <assign|texgraph-plot-surface|<macro|fun-f|start-x|end-x|start-y|end-y|<\plot|<localize|Plot
  surface>>
    <plot-group|<localize|Function>>

    <plot-function|f|<arg|fun-f>>

    <plot-group|<localize|Range>>

    <plot-range|x|<arg|start-x>|<arg|end-x>>

    <plot-range|y|<arg|start-y>|<arg|end-y>>
  </plot>>>

  <assign|texgraph-plot-surface*|<macro|fun-x|fun-y|fun-z|start-u|end-u|start-v|end-v|<\plot|<localize|Plot
  parametric surface>>
    <plot-group|<localize|Function>>

    <plot-function|x|<arg|fun-x>>

    <plot-function|y|<arg|fun-y>>

    <plot-function|z|<arg|fun-z>>

    <plot-group|<localize|Range>>

    <plot-range|u|<arg|start-u>|<arg|end-u>>

    <plot-range|v|<arg|start-v>|<arg|end-v>>
  </plot>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>