<TeXmacs|2.1.1>

<style|<tuple|tmdoc|maxima|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|\<#7F16\>\<#8F91\>\<#4F1A\>\<#8BDD\>>

  Inside input fields of sessions, the cursor keys have a special meaning:
  when moving upwards or downwards, you will move to previous or subsequent
  input fields. When moving to the left or to the right, you will never leave
  the input field; you should rather use the mouse for this.

  \<#5728\>\<#4F1A\>\<#8BDD\>\<#7684\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#5185\>\<#FF0C\>\<#5149\>\<#6807\>\<#952E\>\<#6709\>\<#7279\>\<#6B8A\>\<#7684\>\<#7528\>\<#9014\>\<#FF1A\>\<#4F7F\>\<#7528\>\<#5411\>\<#4E0A\>\<#6216\>\<#5411\>\<#4E0B\>\<#79FB\>\<#52A8\>\<#FF0C\>\<#4F1A\>\<#8BA9\>\<#4F60\>\<#79FB\>\<#52A8\>\<#5230\>\<#4E0A\>\<#4E00\>\<#4E2A\>\<#6216\>\<#4E0B\>\<#4E00\>\<#4E2A\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#FF1B\>\<#4F7F\>\<#7528\>\<#5411\>\<#5DE6\>\<#6216\>\<#5411\>\<#53F3\>\<#79FB\>\<#52A8\>\<#FF0C\>\<#5141\>\<#8BB8\>\<#4F60\>\<#5728\>\<#5F53\>\<#524D\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#79FB\>\<#52A8\>\<#3002\>\<#4F60\>\<#65E0\>\<#6CD5\>\<#4F7F\>\<#7528\>\<#5149\>\<#6807\>\<#952E\>\<#79BB\>\<#5F00\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#FF0C\>\<#8FD9\>\<#662F\>\<#9F20\>\<#6807\>\<#7684\>\<#529F\>\<#80FD\>\<#3002\>

  Some facilities for editing input, output and text fields are available in
  the <menu|Session|Field> menu. Keyboard shortcuts for inserting fields are
  <shortcut|(structured-insert-up)> (insert above) and
  <shortcut|(structured-insert-down)>. Keyboard shortcuts for removing
  matching text/input/output fields are <shortcut|(structured-remove-left)>
  (remove backwards) and <shortcut|(structured-remove-right)> (remove current
  fields).

  <menu|Session|Field>\<#83DC\>\<#5355\>\<#4E2D\>\<#63D0\>\<#4F9B\>\<#4E86\>\<#4E00\>\<#4E9B\>\<#7528\>\<#4E8E\>\<#7F16\>\<#8F91\>\<#8F93\>\<#5165\>\<#3001\>\<#8F93\>\<#51FA\>\<#548C\>\<#6587\>\<#672C\>\<#5B57\>\<#6BB5\>\<#7684\>\<#9009\>\<#9879\>\<#FF0C\>\<#5B83\>\<#4EEC\>\<#90FD\>\<#6709\>\<#7740\>\<#81EA\>\<#5DF1\>\<#5BF9\>\<#5E94\>\<#7684\>\<#5FEB\>\<#6377\>\<#952E\>\<#3002\>
  <shortcut|(structured-insert-up)>\<#548C\><shortcut|(structured-insert-down)>\<#53EF\>\<#7528\>\<#4E8E\>\<#5728\>\<#5F53\>\<#524D\>\<#5B57\>\<#6BB5\>\<#4E0A\>\<#65B9\>\<#6216\>\<#4E0B\>\<#65B9\>\<#63D2\>\<#5165\>\<#5B57\>\<#6BB5\>\<#3002\><shortcut|(structured-remove-left)>\<#53EF\>\<#5220\>\<#9664\>\<#4E0A\>\<#65B9\>\<#7684\>\<#5B57\>\<#6BB5\>\<#FF0C\><shortcut|(structured-remove-right)>\<#53EF\>\<#5220\>\<#9664\>\<#5F53\>\<#524D\>\<#7684\>\<#5B57\>\<#6BB5\>\<#3002\>

  It is possible to create \Psubsessions\Q using <menu|Session|Session|Create
  subsession> or <shortcut|(structured-insert-right)>. In that case, the
  current input-output field becomes the body of an unfolded subsession. Such
  a subsession consists of an explanatory text together with the subsession
  body. Subsessions can be folded and unfolded using
  <shortcut|(dynamic-previous)> <abbr|resp.> <shortcut|(dynamic-next)>.
  Subsessions have a nice rendering on the screen when using the
  <tmpackage|framed-session> package in <menu|Document|Use
  package|Program>.\<#FF08\>\<#4E0D\>\<#592A\>\<#7406\>\<#89E3\>\<#FF09\>

  \<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\><menu|Session|Session commands|Create
  subsession>\<#6216\> <shortcut|(structured-insert-right)>
  \<#521B\>\<#5EFA\>\P\<#5B50\>\<#4F1A\>\<#8BDD\>\Q\<#3002\>\<#5728\>\<#8FD9\>\<#79CD\>\<#60C5\>\<#51B5\>\<#4E0B\>\<#FF0C\>\<#5F53\>\<#524D\>\<#7684\>\<#8F93\>\<#5165\>\<#8F93\>\<#51FA\>\<#5B57\>\<#6BB5\>\<#5C06\>\<#6210\>\<#4E3A\>\<#5C55\>\<#5F00\>\<#5B50\>\<#4F1A\>\<#8BDD\>\<#7684\>\<#4E3B\>\<#4F53\>\<#3002\>\<#8FD9\>\<#6837\>\<#7684\>\<#5B50\>\<#4F1A\>\<#8BDD\>\<#7531\>\<#89E3\>\<#91CA\>\<#6027\>\<#6587\>\<#672C\>\<#548C\>\<#5C55\>\<#5F00\>\<#5B50\>\<#4F1A\>\<#8BDD\>\<#4E3B\>\<#4F53\>\<#7EC4\>\<#6210\>\<#3002\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>
  <shortcut|(dynamic-previous)> \<#548C\> <shortcut|(dynamic-next)>
  \<#5206\>\<#522B\>\<#6298\>\<#53E0\>\<#548C\>\<#5C55\>\<#5F00\>\<#5B50\>\<#4F1A\>\<#8BDD\>\<#3002\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\><menu|Document|Style|Add
  package|Add other package>\<#6DFB\>\<#52A0\><tmpackage| framed-session>
  \<#5B8F\>\<#5305\>\<#4EE5\>\<#589E\>\<#5F3A\>\<#5B50\>\<#4F1A\>\<#8BDD\>\<#7684\>\<#6E32\>\<#67D3\>\<#6548\>\<#679C\>\<#3002\>

  Notice that input/output fields and subsessions are foldable: when clicking
  on the prompt with the mouse, you may fold or unfold the entry to hide or
  show the output. For laptop presentations, this folding and unfolding
  process is done automatically when traversing your presentation. It is also
  possible to fold or unfold all fields in a session using
  <menu|Session|Session|Fold all fields> and <menu|Session|Session|Unfold all
  fields>.<scm|>

  \<#8BF7\>\<#6CE8\>\<#610F\>\<#FF0C\>\<#8F93\>\<#5165\>/\<#8F93\>\<#51FA\>\<#5B57\>\<#6BB5\>\<#548C\>\<#5B50\>\<#4F1A\>\<#8BDD\>\<#662F\>\<#53EF\>\<#6298\>\<#53E0\>\<#7684\>\<#FF1A\>\<#5F53\>\<#7528\>\<#9F20\>\<#6807\>\<#5355\>\<#51FB\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#7684\>\<#63D0\>\<#793A\>\<#7B26\>\<#65F6\>\<#FF0C\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#6298\>\<#53E0\>\<#6216\>\<#5C55\>\<#5F00\>\<#6761\>\<#76EE\>\<#4EE5\>\<#9690\>\<#85CF\>\<#6216\>\<#663E\>\<#793A\>\<#8F93\>\<#51FA\>\<#3002\>\<#5BF9\>\<#4E8E\>\<#6F14\>\<#793A\>\<#6587\>\<#7A3F\>\<#FF0C\>\<#6B64\>\<#6298\>\<#53E0\>\<#548C\>\<#5C55\>\<#5F00\>\<#8FC7\>\<#7A0B\>\<#4F1A\>\<#5728\>\<#4F60\>\<#904D\>\<#5386\>\<#6F14\>\<#793A\>\<#6587\>\<#7A3F\>\<#65F6\>\<#81EA\>\<#52A8\>\<#5B8C\>\<#6210\>\<#3002\>\<#4E5F\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\><menu|Session|Session
  commands|Fold all fields>\<#548C\><menu|Session|Session commands|Unfold all
  fields>\<#6765\>\<#6298\>\<#53E0\>\<#6216\>\<#5C55\>\<#5F00\>\<#4F1A\>\<#8BDD\>\<#4E2D\>\<#7684\>\<#6240\>\<#6709\>\<#5B57\>\<#6BB5\>\<#3002\>

  Other useful editing operations are <menu|Session|Session|Clear all
  fields>, which is useful for creating a demo session which will be executed
  later on, and <menu|Session|Split session>, which can be used for splitting
  a session into parts for inclusion into a paper.

  \<#5176\>\<#4ED6\>\<#6709\>\<#7528\>\<#7684\>\<#7F16\>\<#8F91\>\<#64CD\>\<#4F5C\>\<#8FD8\>\<#6709\><menu|Session|Session
  commands|Clear all fields>\<#548C\><menu|Session|Session commands|Split
  session>\<#FF0C\>\<#524D\>\<#8005\>\<#6709\>\<#52A9\>\<#4E8E\>\<#521B\>\<#5EFA\>\<#7A0D\>\<#540E\>\<#6267\>\<#884C\>\<#7684\>\<#6F14\>\<#793A\>\<#4F1A\>\<#8BDD\>\<#FF0C\>\<#540E\>\<#8005\>\<#53EF\>\<#7528\>\<#4E8E\>\<#5C06\>\<#4F1A\>\<#8BDD\>\<#62C6\>\<#5206\>\<#4E3A\>\<#591A\>\<#4E2A\>\<#90E8\>\<#5206\>\<#FF0C\>\<#4EE5\>\<#4FBF\>\<#5305\>\<#542B\>\<#5728\>\<#8BBA\>\<#6587\>\<#4E2D\>\<#3002\>

  <\example>
    <label|session-example>\<#4E0B\>\<#9762\>\<#7ED9\>\<#51FA\>\<#4E86\>\<#4E00\>\<#4E2A\>\<#5178\>\<#578B\>\<#7684\>
    Maxima \<#4F1A\>\<#8BDD\>\<#3002\>\<#5982\>\<#679C\>\<#4F60\>\<#7684\>\<#7CFB\>\<#7EDF\>\<#4E0A\>\<#5B58\>\<#5728\>
    Maxima\<#FF0C\>\<#90A3\>\<#4E48\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#5C06\>\<#5149\>\<#6807\>\<#653E\>\<#5728\>\<#5176\>\<#4E2D\>\<#4E00\>\<#4E2A\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#4E2D\>\<#FF0C\>\<#6267\>\<#884C\>\<#4E00\>\<#4E9B\>\<#7F16\>\<#8F91\>\<#64CD\>\<#4F5C\>\<#540E\>\<#FF0C\>\<#5C1D\>\<#8BD5\>\<#91CD\>\<#65B0\>\<#6267\>\<#884C\>\<#5B83\>\<#3002\>

    <\session|maxima|default>
      <\output>
        Maxima 5.25.1 http://maxima.sourceforge.net

        using Lisp SBCL 1.0.51

        Distributed under the GNU Public License. See the file COPYING.

        Dedicated to the memory of William Schelter.

        The function bug_report() provides bug reporting information.
      </output>

      <\unfolded-io>
        <with|color|red|(<with|math-font-family|rm|%i>1) >
      <|unfolded-io>
        diff (x^x^x, x)
      <|unfolded-io>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o1>)
        >>x<rsup|x<rsup|x>>*<around*|(|x<rsup|x>*log
        <around*|(|x|)>*<around*|(|log <around*|(|x|)>+1|)>+x<rsup|x-1>|)>>>
      </unfolded-io>

      <\unfolded-io>
        <with|color|red|(<with|math-font-family|rm|%i>2) >
      <|unfolded-io>
        integrate (%o1, x)
      <|unfolded-io>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o2>)
        >>\<mathe\><rsup|\<mathe\><rsup|x*log <around*|(|x|)>>*log
        <around*|(|x|)>>>>
      </unfolded-io>

      <\unfolded-io>
        <with|color|red|(<with|math-font-family|rm|%i>3) >
      <|unfolded-io>
        integrate (x^5 / (x^2 - x + 17), x)
      <|unfolded-io>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o3>)
        >><frac|239*log <around*|(|x<rsup|2>-x+17|)>|2>+<frac|1361*arctan
        <around*|(|<frac|2*x-1|<sqrt|67>>|)>|<sqrt|67>>+<frac|3*x<rsup|4>+4*x<rsup|3>-96*x<rsup|2>-396*x|12>>>
      </unfolded-io>
    </session>
  </example>

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>