<TeXmacs|2.1.1>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|\<#9009\>\<#62E9\>\<#8F93\>\<#5165\>\<#65B9\>\<#5F0F\>>

  By default, <TeXmacs> will attempt to evaluate the input field when
  pressing <key|return>. Multiline input can be created using <key|S-return>.
  Alternatively, when selecting the multiline input mode using
  <menu|Session|Input mode|Multiline input>, the <key|return> key will behave
  as usual and <key|S-return> may be used in order to evaluate the input
  field. Notice finally that certain systems admit built-in heuristics for
  testing whether the input has been completed; if not, then the <key|return>
  may behave as usual.

  \<#9ED8\>\<#8BA4\>\<#60C5\>\<#51B5\>\<#4E0B\>\<#FF0C\>TeXmacs
  \<#5C06\>\<#5728\>\<#6309\>\<#4E0B\> Return
  \<#65F6\>\<#5C1D\>\<#8BD5\>\<#5BF9\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#6C42\>\<#503C\>\<#3002\>\<#4F7F\>\<#7528\>
  <key|S-return> \<#53EF\>\<#4EE5\>\<#521B\>\<#5EFA\>\<#591A\>\<#884C\>\<#8F93\>\<#5165\>\<#3002\>\<#5F53\>\<#4F7F\>\<#7528\>
  <menu|Session|Input options|Multiline input>
  \<#9009\>\<#62E9\>\<#591A\>\<#884C\>\<#8F93\>\<#5165\>\<#6A21\>\<#5F0F\>\<#65F6\>\<#FF0C\>\<#6309\>\<#4E0B\>
  <key|return> \<#952E\>\<#5C06\>\<#6B63\>\<#5E38\>\<#6362\>\<#884C\>\<#FF0C\>\<#5BF9\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#6C42\>\<#503C\>\<#5219\>\<#9700\>\<#8981\>\<#6309\>\<#4E0B\><key|S-return>\<#3002\>\<#6700\>\<#540E\>\<#8BF7\>\<#6CE8\>\<#610F\>\<#FF0C\>\<#67D0\>\<#4E9B\>\<#7CFB\>\<#7EDF\>\<#5141\>\<#8BB8\>\<#5185\>\<#7F6E\>\<#542F\>\<#53D1\>\<#5F0F\>\<#65B9\>\<#6CD5\>\<#6765\>\<#6D4B\>\<#8BD5\>\<#8F93\>\<#5165\>\<#662F\>\<#5426\>\<#5DF2\>\<#5B8C\>\<#6210\>\<#FF1B\>\<#5982\>\<#679C\>\<#4E0D\>\<#662F\>\<#FF0C\>\<#90A3\>\<#4E48\>
  Return \<#53EF\>\<#80FD\>\<#4F1A\>\<#50CF\>\<#5F80\>\<#5E38\>\<#4E00\>\<#6837\>\<#8FD0\>\<#884C\>\<#3002\>

  Certain applications allow you to type the mathematical input in a
  graphical, two dimensional form. This feature can be used by selecting
  <menu|Session|Input mode|Mathematical input>. If this feature is available,
  then it is usually also possible to copy and paste output back into the
  input. However, it depends on the particular application how well this
  works. Keep in mind that some key combinations may be used by the
  <subsubmenu|Session|Input|Mathematical input> mode: for instance the key
  <key|$> is usually redefined inside math mode, so if you want to input it
  you'll have to type <key|S-F5><key|$>. You can read more about the prefix
  key <key|S-F5> in \P<hlink|Keyboard shortcuts for text
  mode|../text/keyboard/man-text-kbd.en.tm>\Q.

  \<#67D0\>\<#4E9B\>\<#5E94\>\<#7528\>\<#7A0B\>\<#5E8F\>\<#5141\>\<#8BB8\>\<#60A8\>\<#4EE5\>\<#56FE\>\<#5F62\>\<#5316\>\<#3001\>\<#4E8C\>\<#7EF4\>\<#5316\>\<#7684\>\<#5F62\>\<#5F0F\>\<#952E\>\<#5165\>\<#6570\>\<#5B66\>\<#8F93\>\<#5165\>\<#3002\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\><menu|Session|Input
  mode|Mathematical input>\<#5F00\>\<#542F\>\<#6B64\>\<#529F\>\<#80FD\>\<#3002\>\<#5982\>\<#679C\>\<#6B64\>\<#529F\>\<#80FD\>\<#53EF\>\<#7528\>\<#FF0C\>\<#5219\>\<#901A\>\<#5E38\>\<#4E5F\>\<#53EF\>\<#4EE5\>\<#5C06\>\<#8F93\>\<#51FA\>\<#590D\>\<#5236\>\<#5E76\>\<#7C98\>\<#8D34\>\<#5230\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#4F7F\>\<#7528\>\<#3002\>\<#4F46\>\<#662F\>\<#FF0C\>\<#8FD9\>\<#53D6\>\<#51B3\>\<#4E8E\>\<#7279\>\<#5B9A\>\<#5E94\>\<#7528\>\<#7A0B\>\<#5E8F\>\<#7684\>\<#5DE5\>\<#4F5C\>\<#60C5\>\<#51B5\>\<#3002\>\<#8BF7\>\<#8BB0\>\<#4F4F\>\<#FF0C\>\<#4E00\>\<#4E9B\>\<#7EC4\>\<#5408\>\<#952E\>\<#53EF\>\<#80FD\>\<#4F1A\>\<#88AB\><subsubmenu|Session|Input|Mathematical
  input>\<#6A21\>\<#5F0F\>\<#5360\>\<#7528\>\<#FF1A\>\<#4F8B\>\<#5982\>\<#FF0C\>\<#952E\>
  <key|$> \<#5728\>\<#6570\>\<#5B66\>\<#6A21\>\<#5F0F\>\<#4E2D\>\<#901A\>\<#5E38\>\<#4F1A\>\<#88AB\>\<#91CD\>\<#5B9A\>\<#4E49\>\<#FF0C\>\<#56E0\>\<#6B64\>\<#FF0C\>\<#5982\>\<#679C\>\<#60F3\>\<#8981\>\<#8F93\>\<#5165\>\<#5B83\>\<#FF0C\>\<#4F60\>\<#5FC5\>\<#987B\>\<#952E\>\<#5165\>
  <key|S-F5><key|$>\<#3002\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#5728\>\P<hlink|Keyboard
  shortcuts for text mode|../text/keyboard/man-text-kbd.en.tm>\Q\<#4E2D\>\<#4E86\>\<#89E3\>\<#5230\>\<#5173\>\<#4E8E\>\<#524D\>\<#7F00\>\<#952E\>
  <key|S-F5> \<#7684\>\<#66F4\>\<#591A\>\<#4FE1\>\<#606F\>\<#3002\>

  <\example>
    Below, you will find the <hlink|previous example
    session|man-session-edit.en.tm#session-example>, but now using
    mathematical input:

    \<#5728\>\<#4E0B\>\<#9762\>\<#FF0C\>\<#4F60\>\<#5C06\>\<#770B\>\<#5230\>\<#4E4B\>\<#524D\>\<#7684\>\<#793A\>\<#4F8B\>\<#4F1A\>\<#8BDD\>\<#FF0C\>\<#4F46\>\<#73B0\>\<#5728\>\<#542F\>\<#7528\>\<#4E86\>\<#6570\>\<#5B66\>\<#8F93\>\<#5165\>\<#FF1A\>

    <\session|maxima|default>
      <\output>
        Maxima 5.25.1 http://maxima.sourceforge.net

        using Lisp SBCL 1.0.51

        Distributed under the GNU Public License. See the file COPYING.

        Dedicated to the memory of William Schelter.

        The function bug_report() provides bug reporting information.
      </output>

      <\unfolded-io-math>
        <with|color|red|(<with|math-font-family|rm|%i>1) >
      <|unfolded-io-math>
        diff<around*|(|x<rsup|x<rsup|x>>,x|)>
      <|unfolded-io-math>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o1>)
        >>x<rsup|x<rsup|x>>*<around*|(|x<rsup|x>*log
        <around*|(|x|)>*<around*|(|log <around*|(|x|)>+1|)>+x<rsup|x-1>|)>>>
      </unfolded-io-math>

      <\unfolded-io-math>
        <with|color|red|(<with|math-font-family|rm|%i>2) >
      <|unfolded-io-math>
        <big|int>%o1*\<mathd\> x
      <|unfolded-io-math>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o2>)
        >>\<mathe\><rsup|\<mathe\><rsup|x*log <around*|(|x|)>>*log
        <around*|(|x|)>>>>
      </unfolded-io-math>

      <\unfolded-io-math>
        <with|color|red|(<with|math-font-family|rm|%i>3) >
      <|unfolded-io-math>
        <big|int><frac|x<rsup|5>|x<rsup|2>-x+17>*\<mathd\> x
      <|unfolded-io-math>
        <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o3>)
        >><frac|239*log <around*|(|x<rsup|2>-x+17|)>|2>+<frac|1361*arctan
        <around*|(|<frac|2*x-1|<sqrt|67>>|)>|<sqrt|67>>+<frac|3*x<rsup|4>+4*x<rsup|3>-96*x<rsup|2>-396*x|12>>>
      </unfolded-io-math>
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