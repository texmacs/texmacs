<TeXmacs|1.99.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Usage of the Python Plugin>

  Separate statements by using <key|shift return>. When you are ready to have
  <name|Python> evaluate your input press <key|return>. \ If you would like
  this behaviour reversed select <menu|Focus|Input options|Multiline input>.

  The plugin has the following features:

  <paragraph|Autocompletion>Type the fist letters of a built-in command or a
  command you have previously defined and then press <key|tab> until you find
  a completion of your choice.

  <paragraph|<name|PostScript> output and <python|matplotlib> support>Use the
  function <python|ps_out (data)> to output <name|PostScript> data directly
  to <TeXmacs>:

    <\itemize>
      <item>If the <verbatim|data> is a string and has more than one line, it
    will be processed as raw <name|PostScript> data.

    <item>If <verbatim|data> is a simple string, it is assumed to contain the
    filename of a <name|PostScript> file which will be read (if the file has
    no extension, the defaults <verbatim|.eps> and <verbatim|.ps> will be
    tried in that order).

    <item>If <verbatim|data> is an instance of
    <python|matplotlib.pyplot.Figure> and <strong|only if additionally> a
    backend supporting output to (encapsulated) <name|PostScript> is being
    used, then the plugin will call <python|data.savefig()> to save the plot
    to a string and copy it into the <TeXmacs> document. See <hlink|the
    examples section|python-demo.en.tm>.

      <item>If <verbatim|data> is a file or other object which provides a
    '<verbatim|read>' method, the <name|PostScript> data will be obtained by
    calling such method.
    </itemize>

  <paragraph|Help window>Type any name followed by a question mark <key|?> to
  have a popup window with the associated <python|help()> as well as the code
  if it is available through <python|inspect.getsource()>. For instance:

  <\session|python|default>
    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      ps_out?
    </input>
  </session>

  <paragraph|Usage as a scripting language>Selecting the menu item
  <menu|Insert|Fold|Executable|Python> will produce the following output:
  <script-input|python|default||>. You may enter any <name|Python> expression
  or statement in the yellow box, e.g <script-input|python|default|1+2|3>.
  Hitting <shortcut|(kbd-return)> inside the box will alternate between the
  unevaluated input and the evaluated output.

  For more complex code you can use <shortcut|(kbd-shift-return)> to enable
  multi-line input. Note however that <em|expressions> (e.g. <python|1+2>),
  <python|>are evaluated using <python|eval()> and the resulting value is
  sent to <TeXmacs>, whereas <em|statements> (e.g. <python|print "hi there">,
  or <python|a=1;a=a+2>) are sent to <python|compile()> and in doing this,
  the return value of the last statement is lost. Therefore, in case you have
  more than one expression you need to write to <python|os.sys.stdout> in
  order to see the output inline: <script-input|python|default|a=1;a=a+2;print
  a|3> will print <script-output|python|default|a=1;a=a+2;print a|3> but
  <script-input|python|default|a=1;a=a+2|3> will print nothing. This
  limitation may be fixed in the future.

  Additionally, after selecting <menu|Document|Scripts|Python>, you may also
  insert <em|executable input fields> using <shortcut|(make-calc-input)> or
  <menu|Insert|Link|Executable input field> (again, pressing <key|return>
  alternates between evaluated and unevaluated input). These have identifiers
  associated which may be referred to in other fields by inserting a
  <em|field reference> using <shortcut|(make 'calc-ref)> or
  <menu|Insert|Link|Field reference>. Plain <em|input fields> are also
  available using <shortcut|(make-calc-inert)> or <menu|Insert|Link|Input
  field>. This allows for dynamic documents which automatically run the
  scripts after changes to the referred fields. See ``Plug-ins as scripting
  languages'' in the <TeXmacs> documentation for more on this topic.

  <tmdoc-copyright|2004, 2014|Adrián Soto|Miguel de Benito Delgado>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
  </collection>
</initial>