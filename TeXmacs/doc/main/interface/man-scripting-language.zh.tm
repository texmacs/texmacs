<TeXmacs|2.1.1>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>>

  <TeXmacs> provides a few other kinds of additional interfaces to external
  systems in addition to shell-like interfaces. First of all, it is possible
  to insert a so called \Pexecutable switch\Q anywhere in the document using
  <menu|Insert|Fold|Executable>.

  \<#9664\>\<#4E86\>\<#7C7B\>\<#4F3C\>\<#4E8E\> shell
  \<#7684\>\<#63A5\>\<#53E3\>\<#FF0C\><TeXmacs>
  \<#8FD8\>\<#4E3A\>\<#5916\>\<#90E8\>\<#7CFB\>\<#7EDF\>\<#63D0\>\<#4F9B\>\<#4E86\>\<#4E00\>\<#4E9B\>\<#5176\>\<#4ED6\>\<#7C7B\>\<#578B\>\<#7684\>\<#9644\>\<#52A0\>\<#63A5\>\<#53E3\>\<#3002\>\<#9996\>\<#5148\>\<#FF0C\>\<#5728\>\<#6587\>\<#6863\>\<#7684\>\<#4EFB\>\<#4F55\>\<#4F4D\>\<#7F6E\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>
  <menu|Insert|Fold|Executable> \<#63D2\>\<#5165\>\<#4E00\>\<#4E2A\>\<#6240\>\<#8C13\>\<#7684\>
  \Pexecutable switch\Q\<#FF08\>\<#53EF\>\<#6267\>\<#884C\>\<#5F00\>\<#5173\>\<#FF09\>\<#3002\>

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

  \<#4F8B\>\<#5982\>\<#FF0C\>\<#5982\>\<#679C\>\<#4F60\>\<#7684\>\<#7CFB\>\<#7EDF\>\<#4E0A\>\<#5B89\>\<#88C5\>\<#4E86\>
  <name|Maxima>\<#FF0C\>\<#90A3\>\<#4E48\>\<#4F7F\>\<#7528\>
  <menu|Insert|Fold|Executable|Maxima> \<#5E94\>\<#8BE5\>\<#4F1A\>\<#4EA7\>\<#751F\>\<#7C7B\>\<#4F3C\>\<#4E8E\>
  <script-input|maxima|default||> \<#7684\>\<#6807\>\<#8BB0\>\<#3002\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#5728\>\<#6807\>\<#8BB0\>\<#7684\>\<#9EC4\>\<#8272\>\<#90E8\>\<#5206\>\<#8F93\>\<#5165\>\<#4E00\>\<#4E2A\>
  <name|Maxima> \<#8868\>\<#8FBE\>\<#5F0F\>\<#FF0C\>\<#4F8B\>\<#5982\><script-input|maxima|default|diff(x^x,x)|>\<#3002\>\<#4F60\>\<#4E5F\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#4F7F\>\<#7528\>
  <shortcut|(kbd-return)> \<#5728\>\<#672A\>\<#6C42\>\<#503C\>\<#7684\>\<#8F93\>\<#5165\>\<#548C\>\<#5DF2\>\<#6C42\>\<#503C\>\<#7684\>\<#8F93\>\<#51FA\>
  <script-output|maxima|default|diff(x^x,x)|<math|x<rsup|x>*<around*|(|log
  <around*|(|x|)>+1|)>>> \<#4E4B\>\<#95F4\>\<#6765\>\<#56DE\>\<#5207\>\<#6362\>\<#3002\>\<#4F60\>\<#8FD8\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>
  <shortcut|(kbd-shift-return)> \<#542F\>\<#7528\>\<#591A\>\<#884C\>\<#8F93\>\<#5165\>\<#3002\>\<#8FD9\>\<#79CD\>\<#53EF\>\<#6267\>\<#884C\>\<#5F00\>\<#5173\>\<#5BF9\>\<#4E8E\><name|
  Dra<TeX>>\<#3001\><name|Eukleides>\<#3001\><name|Feynmf>
  \<#7B49\>\<#63D2\>\<#4EF6\>\<#975E\>\<#5E38\>\<#6709\>\<#7528\>\<#FF0C\>\<#8FD9\>\<#4E9B\>\<#63D2\>\<#4EF6\>\<#4E3B\>\<#8981\>\<#7528\>\<#4E8E\>
  <TeXmacs> \<#6587\>\<#6863\>\<#5185\>\<#90E8\>\<#7684\>\<#9AD8\>\<#6548\>\<#8BA1\>\<#7B97\>\<#548C\>\<#63D2\>\<#5165\>\<#7279\>\<#6B8A\>\<#56FE\>\<#5F62\>\<#3002\>

  Some plug-ins such as <name|Maxima> can even be selected as a <em|scripting
  language> using <menu|Document|Scripts|Maxima>. When doing so, a special
  <menu|Maxima> menu will appear, which allows for many useful operations
  directly on formulas. For instance, when putting the cursor inside the
  formula <math|1+1> and pressing <shortcut|(script-eval)> or
  <menu|Evaluate>, the formula gets evaluated automatically to yield
  <math|2>.

  \<#4F60\>\<#751A\>\<#81F3\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\><menu|Document|Scripts|Maxima>\<#6765\>\<#9009\>\<#62E9\><name|Maxima>\<#6216\>\<#5176\>\<#4ED6\>\<#63D2\>\<#4EF6\>\<#4F5C\>\<#4E3A\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#3002\>\<#5F53\>\<#4F60\>\<#9009\>\<#62E9\><name|Maxima>\<#63D2\>\<#4EF6\>\<#65F6\>\<#FF0C\>\<#4F1A\>\<#51FA\>\<#73B0\>\<#4E00\>\<#4E2A\>\<#7279\>\<#6B8A\>\<#7684\>
  <menu|Maxima> \<#83DC\>\<#5355\>\<#FF0C\>\<#8FD9\>\<#4E2A\>\<#83DC\>\<#5355\>\<#4E2D\>\<#5305\>\<#542B\>\<#4E86\>\<#8BB8\>\<#591A\>\<#76F4\>\<#63A5\>\<#5BF9\>\<#516C\>\<#5F0F\>\<#4F7F\>\<#7528\>\<#7684\>\<#5B9E\>\<#7528\>\<#64CD\>\<#4F5C\>\<#3002\>\<#4F8B\>\<#5982\>\<#FF0C\>\<#5C06\>\<#5149\>\<#6807\>\<#653E\>\<#5728\>\<#6570\>\<#5B66\>\<#6A21\>\<#5F0F\>\<#7684\><math|1+1>\<#5185\>\<#5E76\>\<#4F7F\>\<#7528\><menu|Maxima|Evaluate>\<#FF0C\>\<#8FD9\>\<#4E2A\>\<#516C\>\<#5F0F\>\<#4F1A\>\<#88AB\>\<#81EA\>\<#52A8\>\<#6C42\>\<#503C\>\<#5F97\>\<#51FA\>
  2\<#3002\>

  If a plug-in can be used as a scripting language, then it is possible to
  create executable switches with links between them. More precisely,
  assuming that you selected a scripting language from
  <menu|Document|Scripts>, you may insert a new <em|executable input field>
  using <shortcut|(make-calc-input)> or <menu|Insert|Link|Executable input
  field>. As before, when pressing <key|return>, the current input is
  evaluated and you will see the corresponding output; you may switch back to
  the input by pressing <key|return> once more.

  \<#5982\>\<#679C\>\<#4E00\>\<#4E2A\>\<#63D2\>\<#4EF6\>\<#53EF\>\<#4EE5\>\<#4F5C\>\<#4E3A\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#4F7F\>\<#7528\>\<#FF0C\>\<#90A3\>\<#4E48\>\<#5C31\>\<#53EF\>\<#4EE5\>\<#521B\>\<#5EFA\>\<#53EF\>\<#94FE\>\<#63A5\>\<#7684\>\<#53EF\>\<#6267\>\<#884C\>\<#5F00\>\<#5173\>\<#3002\>\<#4E5F\>\<#5C31\>\<#662F\>\<#8BF4\>\<#FF0C\>\<#5982\>\<#679C\>\<#4F60\>\<#5728\><menu|Document|Scripts>\<#4E2D\>\<#9009\>\<#4E2D\>\<#4E86\>\<#4E00\>\<#79CD\>\<#811A\>\<#672C\>\<#8BED\>\<#8A00\>\<#FF0C\>\<#4F60\>\<#5C31\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\><shortcut|(make-calc-input)>\<#6216\>
  <menu|Insert|Link|Executable input field>\<#63D2\>\<#5165\>\<#4E00\>\<#4E2A\>\<#65B0\>\<#7684\>\<#53EF\>\<#6267\>\<#884C\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#3002\>\<#548C\>\<#4E4B\>\<#524D\>\<#7684\>\<#4E00\>\<#6837\>\<#FF0C\>\<#6309\>\<#4E0B\>
  <key|return> \<#65F6\>\<#FF0C\>\<#4F1A\>\<#5BF9\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#6C42\>\<#503C\>\<#5E76\>\<#7ED9\>\<#51FA\>\<#76F8\>\<#5E94\>\<#8F93\>\<#51FA\>\<#FF1B\>\<#518D\>\<#6B21\>\<#6309\>\<#4E0B\>
  <key|return> \<#FF0C\>\<#4F1A\>\<#5207\>\<#6362\>\<#4E3A\>\<#672A\>\<#6C42\>\<#503C\>\<#7684\>\<#8F93\>\<#5165\>\<#3002\>

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

  \<#FF08\>\<#672A\>\<#80FD\>\<#627E\>\<#5230\>deactivating\<#76F8\>\<#5173\>\<#64CD\>\<#4F5C\>\<#FF0C\>\<#76EE\>\<#524D\>\<#4E3A\>\<#4F9D\>\<#7167\>\<#5B57\>\<#9762\>\<#610F\>\<#601D\>\<#7FFB\>\<#8BD1\>\<#FF09\>

  \<#4E0E\>\<#53EF\>\<#6267\>\<#884C\>\<#5F00\>\<#5173\>\<#4E0D\>\<#540C\>\<#7684\>\<#662F\>\<#FF0C\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#7981\>\<#7528\>\<#5B57\>\<#6BB5\>\<#6216\>\<#7F16\>\<#8F91\>\<#7126\>\<#70B9\>\<#680F\>\<#4E2D\>\<#7684\>
  <samp|Ref> \<#5B57\>\<#6BB5\>\<#6765\>\<#5C06\>\<#6807\>\<#8BC6\>\<#7B26\>\<#9644\>\<#52A0\>\<#5230\>\<#8FD9\>\<#79CD\>\<#53EF\>\<#6267\>\<#884C\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#3002\>\<#5728\>\<#5176\>\<#4ED6\>\<#53EF\>\<#94FE\>\<#63A5\>\<#53EF\>\<#6267\>\<#884C\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#4E2D\>\<#FF0C\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>
  <shortcut|(make 'calc-ref)> \<#6216\> <menu|Insert|Link|Field reference>
  \<#63D2\>\<#5165\>\<#4E00\>\<#4E2A\> <em|field
  reference>\<#FF0C\>\<#6765\>\<#5F15\>\<#7528\>\<#8BE5\>\<#5B57\>\<#6BB5\>\<#7684\>\<#503C\>\<#3002\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\><shortcut|(make-calc-inert)>\<#6216\><menu|Insert|Link|Input
  field>\<#6765\>\<#63D2\>\<#5165\>\<#666E\>\<#901A\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#FF0C\>\<#8FD9\>\<#662F\>\<#4E00\>\<#79CD\>\<#53EF\>\<#6267\>\<#884C\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#7684\>\<#53D8\>\<#4F53\>\<#3002\>\<#666E\>\<#901A\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#53EA\>\<#80FD\>\<#7528\>\<#4F5C\>\<#8F93\>\<#5165\>\<#FF0C\>\<#5728\>\<#8FD9\>\<#7C7B\>\<#5B57\>\<#6BB5\>\<#5185\>\<#6309\>\<#4E0B\><key|return>\<#FF0C\>\<#53EA\>\<#4F1A\>\<#8BA1\>\<#7B97\>\<#5E76\>\<#66F4\>\<#65B0\>\<#90A3\>\<#4E9B\>\<#5F15\>\<#7528\>\<#4E86\>\<#5B83\>\<#7684\>\<#5176\>\<#4ED6\>\<#5B57\>\<#6BB5\>\<#FF0C\>\<#800C\>\<#4E0D\>\<#4F1A\>\<#5BF9\>\<#5B83\>\<#672C\>\<#8EAB\>\<#8FDB\>\<#884C\>\<#6C42\>\<#503C\>\<#3002\>

  <\example>
    The executable input fields may for instance be nice in pedagogic
    documents in which parts of the document may be modified and recomputed
    by the reader. For instance, evaluation of the input fragment

    \<#5728\>\<#6559\>\<#5B66\>\<#6587\>\<#6863\>\<#4E2D\>\<#FF0C\>\<#53EF\>\<#6267\>\<#884C\>\<#8F93\>\<#5165\>\<#5B57\>\<#6BB5\>\<#662F\>\<#5F88\>\<#597D\>\<#7528\>\<#7684\>\<#FF0C\>\<#6587\>\<#6863\>\<#4E2D\>\<#7684\>\<#8FD9\>\<#4E9B\>\<#90E8\>\<#5206\>\<#53EF\>\<#4EE5\>\<#88AB\>\<#8BFB\>\<#8005\>\<#81EA\>\<#7531\>\<#4FEE\>\<#6539\>\<#FF0C\>\<#5E76\>\<#89C2\>\<#5BDF\>\<#91CD\>\<#65B0\>\<#8BA1\>\<#7B97\>\<#7684\>\<#7ED3\>\<#679C\>\<#3002\>\<#4F8B\>\<#5982\>\<#FF0C\>\<#5BF9\>\<#8F93\>\<#5165\>\<#7247\>\<#6BB5\>\<#7684\>\<#6C42\>\<#503C\>\<#FF1A\>

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

    \<#5F53\>\<#7136\>\<#FF0C\>\<#5982\>\<#679C\>\<#8BFB\>\<#8005\>\<#5C06\>\<#8F93\>\<#5165\>\<#51FD\>\<#6570\>
    <math|x<rsup|x>> \<#66F4\>\<#6539\>\<#4E3A\>\<#5176\>\<#4ED6\>\<#5185\>\<#5BB9\>\<#5E76\>\<#6309\>\<#4E0B\>
    <key|return> \<#952E\>\<#FF0C\>\<#90A3\>\<#4E48\>\<#4E00\>\<#9636\>\<#548C\>\<#4E8C\>\<#9636\>\<#5BFC\>\<#6570\>\<#5C06\>\<#88AB\>\<#81EA\>\<#52A8\>\<#66F4\>\<#65B0\>\<#3002\>
  </example>

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

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