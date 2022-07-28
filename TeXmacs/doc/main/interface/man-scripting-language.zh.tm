<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|脚本语言>

  除了类似于 shell 的接口，<TeXmacs>
  还为外部系统提供了一些其他类型的附加接口。首先，在文档的任何位置可以使用
  <menu|Insert|Fold|Executable> 插入一个所谓的
  \P可执行开关\Q（executable switch）。

  例如，如果你的系统上安装了 <name|Maxima>，那么使用
  <menu|Insert|Fold|Executable|Maxima> 应该会产生类似于
  <script-input|maxima|default||> 的标记。你可以在标记的黄色部分输入一个
  <name|Maxima> 表达式，例如<script-input|maxima|default|diff(x^x,x)|>。你也可以通过使用
  <shortcut|(kbd-return)> 在未求值的输入和已求值的输出
  <script-output|maxima|default|diff(x^x,x)|<math|x<rsup|x>*<around*|(|log
  <around*|(|x|)>+1|)>>> 之间来回切换。你还可以通过
  <shortcut|(kbd-shift-return)> 启用多行输入。这种可执行开关对于<name|
  Dra<TeX>>、<name|Eukleides>、<name|Feynmf>
  等插件非常有用，这些插件主要用于 <TeXmacs>
  文档内部的高效计算和插入特殊图形。

  你甚至可以通过<menu|Document|Scripts|Maxima>来选择<name|Maxima>或其他插件作为脚本语言使用。当你选择
  <name|Maxima> 插件时，会出现一个特殊的 <menu|Maxima>
  菜单，这个菜单中包含了许多直接对公式使用的实用操作。例如，将光标放在数学模式的<math|1+1>内并使用<menu|Maxima|Evaluate>，这个公式会被自动求值得出
  2。（原文为pressing <shortcut|(script-eval)> or
  <menu|Evaluate>，但未找到<shortcut|(script-eval)>选项，且按下?键无效，所以进行了修正）

  如果一个插件可以作为脚本语言使用，那么就可以创建可链接的可执行开关。也就是说，如果你在<menu|Document|Scripts>中选中了一种脚本语言，你就可以使用<shortcut|(make-calc-input)>或
  <menu|Insert|Link|Executable input field>插入一个新的可执行输入字段。和之前的一样，按下
  <key|return> 时，会对输入字段求值并给出相应输出；再次按下
  <key|return> ，会切换为未求值的输入。

  与可执行开关不同的是，你可以通过禁用字段或编辑焦点栏中的
  <samp|Ref> 字段来将标识符附加到这种可执行输入字段。在其他可链接可执行输入字段中，你可以通过
  <shortcut|(make 'calc-ref)> 或 <menu|Insert|Link|Field reference>
  插入一个 <em|field reference>，来引用该字段的值。你可以通过<shortcut|(make-calc-inert)>或<menu|Insert|Link|Input
  field>来插入普通输入字段，这是一种可执行输入字段的变体。普通输入字段只能用作输入，在这类字段内按下<key|return>，只会计算并更新那些引用了它的其他字段，而不会对它本身进行求值。（未能找到deactivating相关操作，目前为依照字面意思翻译为禁用字段）

  <\example>
    在教学文档中，可执行输入字段是很好用的，文档中的这些部分可以被读者自由修改，并观察重新计算的结果。例如，对输入片段的求值：

    <\quote-env>
      The derivative of <with|prog-scripts|maxima|<calc-inert|function|<math|<no-break>x<rsup|x>>>>
      equals <with|prog-scripts|maxima|<calc-input|derivative|diff(<calc-ref|function>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)>>>>.

      The second derivative is given by <with|prog-scripts|maxima|<calc-input|second|diff(<calc-ref|derivative>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)><rsup|2>+x<rsup|x-1>>>>.
    </quote-env>

    yields

    <\quote-env>
      The derivative of <with|prog-scripts|maxima|<calc-inert|function2|<math|x<rsup|x>>>>
      equals <with|prog-scripts|maxima|<calc-output|derivative2|diff(<calc-ref|function2>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)>>>>.

      The second derivative is given by <with|prog-scripts|maxima|<calc-output|second2|diff(<calc-ref|derivative2>,x)|<math|x<rsup|x>*<around*|(|log
      <around*|(|x|)>+1|)><rsup|2>+x<rsup|x-1>>>>.
    </quote-env>

    当然，如果读者将输入函数 <math|x<rsup|x>>
    更改为其他内容并按下 <key|return>
    键，那么一阶和二阶导数将被自动更新。
  </example>

  <tmdoc-copyright|1998\U2022|Joris van der Hoeven|詹旭弘>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|prog-scripts|python>
  </collection>
</initial>