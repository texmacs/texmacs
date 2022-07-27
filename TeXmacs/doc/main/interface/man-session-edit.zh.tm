<TeXmacs|2.1.3>

<style|<tuple|tmdoc|maxima|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|编辑会话>

  Inside input fields of sessions, the cursor keys have a special meaning:
  when moving upwards or downwards, you will move to previous or subsequent
  input fields. When moving to the left or to the right, you will never leave
  the input field; you should rather use the mouse for this.

  在会话的输入字段内，光标键有特殊的用途：使用向上或向下移动，会让你移动到上一个或下一个输入字段；使用向左或向右移动，允许你在当前输入字段移动。你无法使用光标键离开输入字段，这是鼠标的功能。

  Some facilities for editing input, output and text fields are available in
  the <menu|Session|Field> menu. Keyboard shortcuts for inserting fields are
  <shortcut|(structured-insert-up)> (insert above) and
  <shortcut|(structured-insert-down)>. Keyboard shortcuts for removing
  matching text/input/output fields are <shortcut|(structured-remove-left)>
  (remove backwards) and <shortcut|(structured-remove-right)> (remove current
  fields).

  <menu|Session|Field>菜单中提供了一些用于编辑输入、输出和文本字段的选项，它们都有着自己对应的快捷键。
  <shortcut|(structured-insert-up)>和<shortcut|(structured-insert-down)>可用于在当前字段上方或下方插入字段。<shortcut|(structured-remove-left)>可删除上方的字段，<shortcut|(structured-remove-right)>可删除当前的字段。

  It is possible to create \Psubsessions\Q using <menu|Session|Session|Create
  subsession> or <shortcut|(structured-insert-right)>. In that case, the
  current input-output field becomes the body of an unfolded subsession. Such
  a subsession consists of an explanatory text together with the subsession
  body. Subsessions can be folded and unfolded using
  <shortcut|(dynamic-previous)> <abbr|resp.> <shortcut|(dynamic-next)>.
  Subsessions have a nice rendering on the screen when using the
  <tmpackage|framed-session> package in <menu|Document|Use
  package|Program>.（不太理解）

  可以使用<menu|Session|Session commands|Create subsession>或
  <shortcut|(structured-insert-right)> 创建\P子会话\Q。在这种情况下，当前的输入输出字段将成为展开子会话的主体。这样的子会话由解释性文本和展开子会话主体组成。可以使用
  <shortcut|(dynamic-previous)> 和 <shortcut|(dynamic-next)>
  分别折叠和展开子会话。可以通过<menu|Document|Style|Add
  package|Add other package>添加<tmpackage| framed-session>
  宏包以增强子会话的渲染效果。

  Notice that input/output fields and subsessions are foldable: when clicking
  on the prompt with the mouse, you may fold or unfold the entry to hide or
  show the output. For laptop presentations, this folding and unfolding
  process is done automatically when traversing your presentation. It is also
  possible to fold or unfold all fields in a session using
  <menu|Session|Session|Fold all fields> and <menu|Session|Session|Unfold all
  fields>.<scm|>

  请注意，输入/输出字段和子会话是可折叠的：当用鼠标单击输入字段的提示符时，你可以折叠或展开条目以隐藏或显示输出。对于演示文稿，此折叠和展开过程会在你遍历演示文稿时自动完成。也可以使用<menu|Session|Session
  commands|Fold all fields>和<menu|Session|Session commands|Unfold all
  fields>来折叠或展开会话中的所有字段。

  Other useful editing operations are <menu|Session|Session|Clear all
  fields>, which is useful for creating a demo session which will be executed
  later on, and <menu|Session|Split session>, which can be used for splitting
  a session into parts for inclusion into a paper.

  其他有用的编辑操作还有<menu|Session|Session commands|Clear all
  fields>和<menu|Session|Session commands|Split
  session>，前者有助于创建稍后执行的演示会话，后者可用于将会话拆分为多个部分，以便包含在论文中。

  <\example>
    <label|session-example>下面给出了一个典型的 Maxima
    会话。如果你的系统上存在 Maxima，那么你可以将光标放在其中一个输入字段中，执行一些编辑操作后，尝试重新执行它。

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