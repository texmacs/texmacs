<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|脚本语言>

  <TeXmacs> provides a few other kinds of additional interfaces to external
  systems in addition to shell-like interfaces. First of all, it is possible
  to insert a so called \Pexecutable switch\Q anywhere in the document using
  <menu|Insert|Fold|Executable>.

  除了类似于 shell 的接口，<TeXmacs>
  还为外部系统提供了一些其他类型的附加接口。首先，在文档的任何位置可以使用
  <menu|Insert|Fold|Executable> 插入一个所谓的 \Pexecutable
  switch\Q（可执行开关）。

  For instance, if <name|Maxima> is installed on your system, then
  <menu|Insert|Fold|Executable|Maxima> should yield something like
  <script-input|maxima|default||>. You may enter a<nbsp><name|Maxima>
  expression in the yellow part of this markup, say
  <script-input|maxima|default|diff(x^x,x)|>. Using <shortcut|(kbd-return)>,
  you may now switch back and forth between the unevaluated input and the
  evaluated output <script-output|maxima|default|diff(x^x,x)|<math|x<rsup|x>*<around*|(|log
  <around*|(|x|)>+1|)>>>. Using <shortcut|(kbd-shift-return)>, you enable
  multi-line input. This kind of executable switches are very useful for
  plug-ins such as <name|Dra<TeX>>, <name|Eukleides>, <name|Feynmf>,
  <abbr|etc.>, which are mainly used for the efficient computation and
  insertion of special graphics inside <TeXmacs> documents.

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

  Some plug-ins such as <name|Maxima> can even be selected as a <em|scripting
  language> using <menu|Document|Scripts|Maxima>. When doing so, a special
  <menu|Maxima> menu will appear, which allows for many useful operations
  directly on formulas. For instance, when putting the cursor inside the
  formula <math|1+1> and pressing <shortcut|(script-eval)> or
  <menu|Evaluate>, the formula gets evaluated automatically to yield
  <math|2>.

  你甚至可以通过<menu|Document|Scripts|Maxima>来选择<name|Maxima>或其他插件作为脚本语言。当你选择<name|Maxima>插件时，会出现一个特殊的
  <menu|Maxima> 菜单，这个菜单中包含了许多直接对公式使用的实用操作。例如，将光标放在数学模式的<math|1+1>内并使用<menu|Maxima|Evaluate>，这个公式会被自动求值得出
  2。

  If a plug-in can be used as a scripting language, then it is possible to
  create executable switches with links between them. More precisely,
  assuming that you selected a scripting language from
  <menu|Document|Scripts>, you may insert a new <em|executable input field>
  using <shortcut|(make-calc-input)> or <menu|Insert|Link|Executable input
  field>. As before, when pressing <key|return>, the current input is
  evaluated and you will see the corresponding output; you may switch back to
  the input by pressing <key|return> once more.

  如果一个插件可以作为脚本语言使用，那么就可以创建可链接的可执行开关。也就是说，如果你在<menu|Document|Scripts>中选中了一种脚本语言，你就可以使用<shortcut|(make-calc-input)>或
  <menu|Insert|Link|Executable input field>插入一个新的可执行输入字段。和之前的一样，按下
  <key|return> 时，会对输入字段求值并给出相应输出；再次按下
  <key|return> ，会切换为未求值的输入。

  Contrary to executable switches, you may attach an identifier to the
  executable input field by deactivating the field or by editing the
  <samp|Ref> field in the focus bar. Inside other executable input fields,
  you may then refer to the value of the field by inserting a <em|field
  reference> using <shortcut|(make 'calc-ref)> or <menu|Insert|Link|Field
  reference>. As a variant to executable input fields, you may sometimes
  prefer to insert plain <em|input fields> using <shortcut|(make-calc-inert)>
  or <menu|Insert|Link|Input field>. These fields can only be used as inputs
  and pressing <key|return> inside such a field will only recompute those
  other fields which depend on it.

  （未能找到deactivating相关操作，目前为依照字面意思翻译）

  与可执行开关不同的是，你可以通过禁用字段或编辑焦点栏中的
  <samp|Ref> 字段来将标识符附加到这种可执行输入字段。在其他可链接可执行输入字段中，你可以通过
  <shortcut|(make 'calc-ref)> 或 <menu|Insert|Link|Field reference>
  插入一个 <em|field reference>，来引用该字段的值。你可以通过<shortcut|(make-calc-inert)>或<menu|Insert|Link|Input
  field>来插入普通输入字段，这是一种可执行输入字段的变体。普通输入字段只能用作输入，在这类字段内按下<key|return>，只会计算并更新那些引用了它的其他字段，而不会对它本身进行求值。

  <\example>
    The executable input fields may for instance be nice in pedagogic
    documents in which parts of the document may be modified and recomputed
    by the reader. For instance, evaluation of the input fragment

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

    Of course, if the reader changes the input function <math|x<rsup|x>> into
    something else and presses <key|return>, then the first and second
    derivatives will be updated automatically.

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