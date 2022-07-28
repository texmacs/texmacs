<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|选择输入方式>

  默认情况下，TeXmacs 将在按下 <key|return>
  时尝试对输入字段求值。使用 <key|S-return>
  可以创建多行输入。当使用 <menu|Session|Input options|Multiline
  input> 选择多行输入模式时，按下 <key|return>
  键将正常换行，对输入字段求值则需要按下<key|S-return>。最后请注意，某些系统允许内置启发式方法来测试输入是否已完成；如果不是，那么
  <key|return> 可能会像往常一样运行。(不太理解什么是built-in
  heuristics，故先直译为内置启发式方法)

  某些应用程序允许您以图形化、二维化的形式键入数学输入。可以通过<menu|Session|Input
  mode|Mathematical input>开启此功能。如果此功能可用，则通常也可以将输出复制并粘贴到输入字段使用。但是，这取决于特定应用程序的工作情况。请记住，一些组合键可能会被<subsubmenu|Session|Input|Mathematical
  input>模式占用：例如，键 <key|$>
  在数学模式中通常会被重定义，因此，如果想要输入它，你必须键入
  <key|S-F5><key|$>。你可以在\P<hlink|文本模式下的快捷键|../text/keyboard/man-text-kbd.zh.tm>\Q中了解到关于前缀键
  <key|S-F5> 的更多信息。

  <\example>
    在下面，你将看到<hlink|之前的示例会话|man-session-edit.zh.tm#session-example>，但现在启用了数学输入：

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

  <tmdoc-copyright|1998\U2022|Joris van der Hoeven|詹旭弘>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>