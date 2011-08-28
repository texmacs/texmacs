<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Writing a simple style package>

  Let us explain on an example how to write a simple style package. First of
  all, you have to create a new buffer using <menu|File|New> and select the
  <tmstyle|source> document style using <menu|Document|Style|source>. Now
  save your empty style package in your personal style package directory

  <verbatim| \ \ \ $HOME/.TeXmacs/packages>

  Notice that the button <menu|Texts> in the file browser corresponds to the
  directory

  <verbatim| \ \ \ $HOME/.TeXmacs/texts>

  Consequently, you can go to the style package directory from there, by
  double clicking on <verbatim|..> and next on <verbatim|packages>.
  Similarly, the directory

  <verbatim| \ \ \ $HOME/.TeXmacs/styles>

  contains your personal style files. After saving your empty style package,
  it should automatically appear in the <menu|Document|Package> menu. Notice
  that style files must be saved using the <verbatim|.ts> file extension. If
  you save the style file in a subdirectory of
  <verbatim|$HOME/.TeXmacs/packages>, then it will automatically appear in
  the corresponding submenu of <menu|Document|Package>.

  Let us now create a simple macro <markup|hi> which displays ``Hello
  world''. First type <shortcut|(make 'assign)>, so as to create an assignment. You should
  see something like

  <\tm-fragment>
    <inactive*|<assign||>>
  </tm-fragment>

  Now enter ``hi'' as the first argument and type <shortcut|(make 'macro)> inside the second
  argument in order to create a macro. You should now see something like

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|>>>
  </tm-fragment>

  Finally, type the text ``Hello world'' in the body of the macro. Your
  document should now consist of the following line:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world>>>
  </tm-fragment>

  After saving your style package, opening a new document and selecting your
  package in the <menu|Document|Use package> menu, you may now use the macro
  <markup|hi> in your document by typing <key|\\ h i> and hitting
  <shortcut|(kbd-return)>.

  In a similar way, you may create macros with arguments. For instance,
  assume that we started entering a macro <markup|hello> in a similar way as
  above. Instead of typing ``Hello world'', we first type <shortcut|(structured-insert-left)>
  inside the macro body so as to create an additional argument on the left
  hand side of the cursor. We next enter the name of the argument, say
  ``name''. You should now see something like below:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|>>>
  </tm-fragment>

  In the second argument of the body, we now type ``Hello '', <shortcut|(make 'arg)>,
  ``name'', <key|right> and ``, how are you today?''. After this you
  should see

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <arg|name>, how are you
    today?>>>
  </tm-fragment>

  The <shortcut|(make 'arg)> shortcut is used to retrieve the macro argument
  <src-arg|name>. Instead of typing <shortcut|(make 'arg)>, ``name'' and
  <key|right>, you may also use the hybrid <key|\\>-key and type
  <key|\\ n a m e> followed by <shortcut|(kbd-return)>. After saving your style
  package, you may again use the macro in any document which uses your
  package by typing <key|\\ h e l l o> and hitting <key|return>.

  From the internal point of view, all macro definitions are stored in the
  environment of the <TeXmacs> typesetter. Besides macros, the environment
  also contains normal environment variables, such as section counters or the
  font size. The environment variables can either be globally changed using
  the <markup|assign> primitive, or locally, using the <markup|with>
  primitive. For instance, when including the line

  <\tm-fragment>
    <inactive*|<assign|section-nr|-1>>
  </tm-fragment>

  in your package, and using <tmstyle|article> as your major style, then the
  first section will be numbered <no-break><with|mode|math|0>. Similarly, the
  variant

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello
    <with|font-shape|small-caps|<arg|name>>!>>>
  </tm-fragment>

  of the <markup|hello> macro displays the name of the person in
  <with|font-shape|small-caps|Small Capitals>. Notice that the <markup|with>
  primitive can also be used to locally redefine a macro. This is for
  instance used in the definitions of the standard list environments, where
  the macro which renders list icons is changed inside the body of the list.
  Yet another variant of the <markup|hello> macro relies on the standard
  <markup|person> macro:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <person|<arg|name>>!>>>
  </tm-fragment>

  In order to produce the macro application <inactive*|<person|<arg|name>>>,
  you first have to start a compound tag using <shortcut|(make 'compound)>, type the name
  ``person'', insert an argument <shortcut|(structured-insert-right)>, and enter the argument
  <src-arg|name> as before. When you are done, you may press
  <shortcut|(kbd-return)> in order to change the <markup|compound> tag into a
  <markup|person> tag. Alternatively, you may type <key|\\>, ``person'',
  <shortcut|(structured-insert-right)> and ``name''.

  By combining the above constructs, an ordinary user should already be able
  to produce style packages for all frequently used notations. An interesting
  technique for writing macros which involve complex formulas with some
  subformulas which may change goes as follows:

  <\enumerate>
    <item>Type the formula, say <with|mode|math|(a<rsub|1>,\<ldots\>,a<rsub|n>)>,
    in an ordinary document.

    <item>Create the skeleton of your macro in your style package:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|>>>
    </tm-fragment>

    <item>Copy the formula and paste it into the body of your macro:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|(a<rsub|1>,\<ldots\>,a<rsub|n>)>>>
    </tm-fragment>

    <item>Replace the subformulas you want to parameterize by macro
    arguments:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|(<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>)>>>
    </tm-fragment>

    <item>You may now use the macro in documents which use your package:

    <\equation*>
      <with|n-tuple|<macro|a|(<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>)>|<n-tuple|a>=<n-tuple|b>.>
    </equation*>
  </enumerate>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>