<TeXmacs|1.0.7.1>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|texgraph-scripts|1.0>

    <\src-purpose>
      Complements of scripts-1.0 for TeXgraph
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

  <\active*>
    <\src-comment>
      Macros for the option "keep evaluated expressions"
    </src-comment>
  </active*>

  <assign|script-assign|<macro|<with|color|dark blue|\<assign\>>>>

  <assign|script-result|<macro|in|out|<arg|in><with|color|dark
  blue|=><arg|out>>>

  <assign|script-approx|<macro|in|out|<arg|in><with|color|dark
  blue|\<approx\>><arg|out>>>

  <drd-props|script-result|arity|2|border|no>

  <drd-props|script-approx|arity|2|border|no>

  <\active*>
    <\src-comment>
      Macros for status information
    </src-comment>
  </active*>

  <assign|script-status|<macro|x|<with|color|red|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<table|<row|<cell|<arg|x>>>>>>>>>

  <assign|script-busy|<macro|<script-status|<localize|Busy>...>>>

  <assign|script-interrupted|<macro|<script-status|<math|\<lightning\>>
  <localize|Interrupted>>>>

  <assign|script-dead|<macro|<script-status|<math|\<lightning\>>
  <localize|Dead>>>>

  <\active*>
    <\src-comment>
      Script rendering.
    </src-comment>
  </active*>

  <assign|script-aux-1|<macro|lan|body|<arg|body>>>

  <assign|script-aux-2|<macro|lan|body|<with|mode|prog|prog-language|<arg|lan>|<arg|body>>>>

  <assign|script-aux-3|<macro|lan|body|<style-with|src-compact|none|<compound|<if|<equal|<value|mode>|math>|script-aux-1|script-aux-2>|<arg|lan>|<arg|body>>>>>

  <assign|render-big-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|#8080A0>|<cwith|2|2|1|1|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-bsep|0.15fn>|<cwith|1|1|1|1|cell-tsep|0.15fn>|<cwith|2|2|1|1|cell-width|1par>|<cwith|2|2|1|1|cell-hmode|min>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|2|2|1|1|cell-tsep|0.3fn>|<cwith|2|2|1|1|cell-bsep|0.3fn>|<cwith|1|1|1|1|cell-halign|c>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|white|<change-case|<arg|lan>|Upcase>>>>>>|<row|<\cell>
    <with|color|black|par-par-sep|0fn|<arg|body>>
  </cell>>>>>>>>

  <assign|render-small-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|#8080A0>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-lsep|0.15fn>|<cwith|1|1|1|1|cell-rsep|0.15fn>|<table|<row|<cell|<small|<with|mode|text|font-family|ss|<with|color|white|<change-case|<arg|lan>|Upcase>>>>>|<cell|<with|color|black|<arg|body>>>>>>>>>>

  <assign|render-eval-script|<macro|lan|body|<with|color|grey|<tabular|<tformat|<cwith|1|-1|1|-1|cell-lborder|0.5ln>|<cwith|1|-1|1|-1|cell-rborder|0.5ln>|<cwith|1|-1|1|-1|cell-bborder|0.5ln>|<cwith|1|-1|1|-1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <with|color|black|par-par-sep|0fn|<arg|body>>
  </cell>>>>>>>>

  <\active*>
    <\src-comment>
      Scripts for different types of languages.
    </src-comment>
  </active*>

  <assign|script-input|<macro|lan|session|in|out|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<provides|<merge|<arg|lan>|-script-input>>|<merge|<arg|lan>|-script-input>|generic-script-input>>|<arg|lan>|<arg|session>|<arg|in>|<arg|out>>>>>

  <assign|generic-script-input|<macro|lan|session|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|lan>|<script-aux-3|<arg|lan>|<arg|in>>>>>>

  <assign|script-output|<macro|lan|session|in|out|<arg|out>>>

  <assign|script-eval|<macro|in|<render-eval-script|<value|prog-scripts>|<script-aux-3|<value|prog-scripts>|<arg|in>>>>>

  <\active*>
    <\src-comment>
      Plots.
    </src-comment>
  </active*>

  <assign|plot|<macro|name|body|<render-big-script|<arg|name>|<arg|body>>>>

  <assign|plot-group|<macro|text|<style-with|src-compact|none|<resize|<small|<with|font-shape|italic|font-series|bold|<arg|text>>>||b-0.3fn||t+0.3fn|>>>>

  <assign|plot-input-field|<macro|size|body|<with|color|grey|<tabular|<tformat|<cwith|1|1|1|1|cell-background|white>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-width|<arg|size>>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<cell|<with|color|black|<arg|body>>>>>>>>>>

  <assign|plot-function|<macro|x|body|<style-with|src-compact|none|<resize|<math|<arg|x>>
  |||3fn||><plot-input-field|0.6par|<math|<arg|body>>>>>>

  <assign|plot-range|<macro|x|start|end|<style-with|src-compact|none|<resize|<math|<arg|x>>:
  |||1.5fn||><plot-input-field|0.25par|<math|<arg|start>>> --
  <plot-input-field|0.25par|<math|<arg|end>>>>>>

  <assign|texgraph-plot-settings|<macro|settings|fun-f|<style-with|src-compact|none|<resize|
  |||3fn||><resize|<small|<with|font-shape|italic|réglages pour <arg|fun-f>>>
  : |||7fn||><plot-input-field|0.47par|<verbatim|<arg|settings>>>>>>

  <assign|texgraph-plot-choices-3|<macro|title1|choice1|title2|choice2|title3|choice3|<style-with|src-compact|none|<resize|<arg|title1>:
  |||8fn||><plot-input-field|0.075par|<arg|choice1>><resize| <arg|title2>:
  |||7fn||><plot-input-field|0.075par|<math|<arg|choice2>>><resize|
  <arg|title3>: |||7fn||><plot-input-field|0.075par|<math|<arg|choice3>>>>>>

  <assign|texgraph-plot-choices-2|<macro|title4|choice4|title5|choice5|<style-with|src-compact|none|<resize|<arg|title4>:
  |||8fn||><plot-input-field|0.075par|<arg|choice4>><resize| <arg|title5>:
  |||7fn||><plot-input-field|0.3par|<verbatim|<arg|choice5>>>>>>

  <assign|texgraph-plot-commands|<\macro|commands>
    <plot-input-field|0.65par|<verbatim|<arg|commands>>>
  </macro>>

  <assign|texgraph-plot-infos|<macro|text|<style-with|src-compact|none|<resize|
  |||3fn||><style-with|src-compact|none|<resize|<small|<arg|text>>||b-0.3fn||t+0.3fn|>>>>>

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
    (+i*haut.)>|<arg|size>|<small|Echelle sur <math|O
    x>>|<arg|scale-x>|<small|Echelle sur <math|O y>>|<arg|scale-y>>

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

  \;

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

  <assign|plot-output|<macro|in|out|<arg|out>>>

  \;

  <\active*>
    <\src-comment>
      Converters.
    </src-comment>
  </active*>

  <assign|converter-input|<macro|format|in|out|<style-with|src-compact|none|<compound|<if|<equal|<get-label|<arg|in>>|document>|render-big-script|render-small-script>|<arg|format>|<with|mode|prog|prog-language|verbatim|<arg|in>>>>>>

  <assign|converter-output|<macro|format|in|out|<arg|out>>>

  <assign|converter-eval|<macro|format|in|<style-with|src-compact|none|<render-small-script|<arg|format>|<with|mode|prog|prog-language|verbatim|<arg|in>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>