<TeXmacs|1.0.3.3>

<style|tmweb>

<\body>
  <tmweb-current|Help|FAQ><tmweb-title|Frequently asked
  questions|<tmweb-help-links>>

  <section*|Overview>

  <\itemize>
    <item>General

    <\itemize-minus>
      <item><hlink|What is <TeXmacs>|#general-1>?

      <item><hlink|For what typical purposes do people use
      \ <TeXmacs>|#general-2>?

      <item><hlink|What Operation Systems are supported|#general-3>?

      <item><hlink|What are the system requirements|#general-4>?

      <item><hlink|I have already learned <TeX>/<LaTeX> and <name|Emacs>, do
      I have to learn all over again|#general-5>?

      <item><hlink|I have a question, where should I ask it|#general-5>?

      <item><hlink|Is it free Software|#general-6>?
    </itemize-minus>

    <item>System Appearance and Behavior

    <\itemize-minus>
      <item><hyper-link|<TeXmacs> hangs when opening a document for a while
      and my disk is being filled with files|#sys-1>?

      <item><hyper-link|How can I see the <LaTeX> or <TeX> code corresponding
      to what I see on the screen|#sys-2>?

      <item><hyper-link|Why don't you use a common graphical user interface
      like GTK for your scrollbars, menus, and so on|#sys-3>?

      <item><hyper-link|Trying to start <TeXmacs> remotely over a ssh
      connection, I get an error, while <abbr|e.g.> xclock works without a
      problem|#sys-4>?
    </itemize-minus>

    <item>Windows/Cygwin specific

    <\itemize-minus>
      <item><hlink|What to do with Cygwin specific questions|#cygwin-1>?

      <item><hlink|How to associate .tm files with <TeXmacs>|#cygwin-2>?

      <item><hlink|How to get spell checking working|#cygwin-3>?
    </itemize-minus>

    <item>Usage

    <\itemize-minus>
      <item><hlink|How to mark/select a whole paragraph|#usage-1>?

      <item><hlink|Spell checking always starts at the beginning of the
      document|#usage-2>?

      <item><hlink|How can I insert an OpenOffice.org table|#usage-3>?
    </itemize-minus>

    <item>Microtypography

    <\itemize-minus>
      <item><hlink|There is too much space after an abbreviation like
      <abbr|wrt.> or <abbr|etc.>|#typo-1>?

      <item><hlink|How to add unbreakable space|#typo-2>?
    </itemize-minus>

    <item>Legacy Questions

    <\itemize-minus>
      <item><hyper-link|A publisher sent me a giant <LaTeX> preamble I'm
      supposed to put in in order to prepare a book for them. What is the
      best way of putting it in and figuring if it will work|#legacy-1>?
    </itemize-minus>
  </itemize>

  <section*|General>

  <\question>
    <label|general-1>What is <TeXmacs>?
  </question>

  <\answer>
    GNU <TeXmacs>

    <\itemize-dot>
      <item>is a free scientific text editor, which was both inspired by
      <TeX> and GNU Emacs.

      <item>allows you to write structured documents via a wysiwyg
      (what-you-see-is-what-you-get) and user friendly interface.

      <item>lets you create new styles.

      <item>implements high-quality typesetting algorithms and <TeX> fonts,
      which helps you to produce professionally looking documents.

      <item>is suitable as an interface for computer algebra systems, as the
      high typesetting quality goes through for automatically generated
      formulas.

      <item>can be highly customized as it supports the
      <name|Guile>/<name|Scheme> extension language.

      <item>lets you export your documents to PS and PDF and offers both
      import and export to HTML, <LaTeX>, Scheme, Verbatim and Xml. We would
      very much appreciate your <hlink|help|http://www.texmacs.org/Web/Contribute.html>
      for writing and improving converters for <TeXmacs> documents.
    </itemize-dot>
  </answer>

  <\question>
    <label|general-2>For what typical purposes do people use \ <TeXmacs>?
  </question>

  <\answer>
    \ <TeXmacs> can be used for

    <\itemize>
      <item>Books and Articles. They can be written fully within <TeXmacs>.
      If your publisher requires a certain <LaTeX> style for an article, then
      as a last step you can export your document to <LaTeX> and make the
      final modifications there.

      <item>Presentations.

      <item>Interface to computer algebra systems and other scientific
      software.

      <item>Webpages. For example the <TeXmacs> webpage is maintained as
      <TeXmacs> documents, which are exported to <name|HTML>.\ 
    </itemize>
  </answer>

  <\question>
    <label|general-3>What Operation Systems are supported?
  </question>

  <\answer>
    <TeXmacs> can be easily installed on all major systems. There are
    <name|rpm> and Debian packages as well as binaries available for
    <name|Linux>. For Mac <name|OS X> there is a <name|Fink> package. For
    <name|MS Windows>, a <name|Cygwin> package is available.\ 

    Work is in progress to rewrite the graphical user interface of <TeXmacs>,
    so that it becomes more portable. Your
    <hlink|help|http://www.texmacs.org/Web/Contribute.html> might actually be
    very useful here.
  </answer>

  <\question>
    <label|general-4>What are the system requirements?
  </question>

  <answer|A reasonably fast machine is recommended. Having said this, I
  (Andreas) am typing this on a Pentium III 450Mhz under Cygwin, and
  <TeXmacs> is still useable here, although not overly reactive. >

  <\question>
    <label|general-5>I have already learned <TeX>/<LaTeX> and <name|Emacs>,
    do I have to learn all over again?
  </question>

  <answer|You will find out, that much <LaTeX>-knowledge can be reused. For
  example, you can start a section by typing <key|\\section[return]> or you
  get <with|mode|math|\<alpha\><rsub|1>> by typing <key|$\\alpha_1$>. Note
  that there are even shorter ways provided, for example <key|$a[tab]$> gives
  you an alpha as well, so you might want to migrate after a while. Styles
  like article, book or seminar, as known from <LaTeX>, are provided as well.
  Furthermore, many <name|Emacs> shortcuts like <key|C-x C-s> for saving a
  file, or <key|C-_> for undo will work. >

  <\question>
    <label|general-6>I have a question, where should I ask it?
  </question>

  <answer|The best place is the <TeXmacs> user mailing list. Search this list
  and the FAQ beforehand, to ensure your question was not asked before.>

  <\question>
    <label|general-7>Is it free Software?
  </question>

  <answer|<TeXmacs> falls under the GNU public licence. >

  <section*|System Appearance and Behavior>

  <\question>
    <label|sys-1><TeXmacs> hangs when opening a document for a while and my
    disk is being filled with files?
  </question>

  <answer|This behaviour is normal. <TeXmacs> calls <name|Metafont> in order
  to generate fonts which are not yet present. The first time you launch
  <TeXmacs>, many fonts may therefore have to be generated. In order to avoid
  this, you may download some <hlink|pregenerated
  fonts|http://www.texmacs.org/Download/Fonts.html>.>

  <\question>
    <label|sys-2>How can I see the <LaTeX> or <TeX> code corresponding to
    what I see on the screen?
  </question>

  <answer|This question is due to a fundamental misunderstanding about
  <TeXmacs>. Indeed, <TeXmacs> is not <em|based> on <TeX>/<LaTeX>, although
  it does support (not yet perfect) <em|conversion> to and from <LaTeX>.
  Furthermore, in theory at least, there is actually no need anymore to look
  at something like the <TeX> source, since <TeXmacs> is guaranteed to be
  fully WYSIWYG. Conversion to <LaTeX> may only be useful, when transmitting
  an accepted paper to the publisher of a journal.>

  <\question>
    <label|sys-3>Why don't you use a common graphical user interface like GTK
    for your scrollbars, menus, and so on?
  </question>

  <\answer>
    When I (Joris) started to develop <TeXmacs> about four years ago, the
    common graphical user interfaces were not as good as nowadays. Moreover,
    I wanted the GUI to support some special features, like <TeX> fonts in
    the menus. Nevertheless, now that graphical user interfaces did become
    much better, I plan to switch to guile-gtk as soon as possible. Using
    Guile-gtk in combination with <TeXmacs> has three main advantages:

    <\enumerate>
      <item>One has full access to the GTK widget set, which includes menus,
      scrollable windows, file choosers, iconbars, etc.

      <item>Guile-gtk provides you with a very flexible and customizable way
      to use these widgets.

      <item>The incorporation of Guile-gtk in <TeXmacs> should be natural,
      since <TeXmacs> already supports the <name|Guile>/<name|Scheme>
      extension language.
    </enumerate>
  </answer>

  <\question>
    <label|sys-4>Trying to start <TeXmacs> remotely over a ssh connection, I
    get an error, while <abbr|e.g.> xclock works without a problem?
  </question>

  <\answer>
    If you get an error message including the following:

    <verbatim|Fatal error: I failed to connect to Xserver in
    'x_display_rep::x_display_rep>

    then execute <verbatim|export DISPLAY=127.0.0.1:10.0> on the remote
    machine and make sure your /etc/hosts file is sound, <abbr|i.e.> it
    contains the line <verbatim|127.0.0.1 localhost> and the IP information
    about the local and remote machine.
  </answer>

  <section*|Windows/Cygwin specific>

  <\question>
    <label|cygwin-1>What to do with Cygwin specific questions?
  </question>

  <\answer>
    Look at the Cygwin FAQ at <hlink|http://cygwin.com/faq.html|http://cygwin.com/faq.html>,
    the Cygwin User Guide at <hlink|http://cygwin.com/cygwin-ug-net/|http://cygwin.com/cygwin-ug-net/>
    and search the Cygwin mailing list <hlink|http://www.cygwin.com/ml/cygwin/|http://www.cygwin.com/ml/cygwin/>.
  </answer>

  <\question>
    <label|cygwin-2>How to associate .tm files with <TeXmacs>?
  </question>

  <\answer>
    Create a file <verbatim|texmacs.bat> with the following content:

    <\verbatim>
      \ \ \ \ rem cmdow @ /hid<next-line> \ \ \ c:\\cygwin\\bin\\bash --login
      -c "texmacs \\"`cygpath -u "%1"`\\""
    </verbatim>

    Ensure that it lies in your <verbatim|PATH>. Now you can associate .tm
    files with this batch file.

    If you want to hide the black Cygwin window when <TeXmacs> is started,
    then download <name|cmdow> from <simple-link|http://www.commandline.co.uk/cmdow/>,
    drop <verbatim|cmdow.exe> <abbr|e.g.> in your
    <verbatim|C:\\WINDOWS\\system32> directory (this applies to <name|Windows
    XP> installed on <verbatim|C:\\>) and uncomment (<abbr|i.e.> remove
    ``rem'' from) the first line of <verbatim|texmacs.bat>.
  </answer>

  <\question>
    <label|cygwin-3>How to get spell checking working?
  </question>

  <\answer>
    Install the Cygwin package aspell. Execute in a shell:

    <\verbatim>
      \ \ \ \ cd /usr/bin<next-line> \ \ \ ln -s /usr/share/aspell/ispell
      ispell
    </verbatim>
  </answer>

  <section*|Usage>

  <\question>
    <label|usage-1>How to mark/select a whole paragraph?
  </question>

  <answer|Clicking once on a word will place the cursor there, clicking twice
  marks this word, clicking thrice will mark the paragraph and so on;
  eventually, the whole document will be selected.>

  <\question>
    <label|usage-2>Spell checking always starts at the beginning of the
    document?
  </question>

  <answer|Mark a region, and spell checking will be restricted to this
  selection.>

  <\question>
    <label|usage-3>How can I insert an OpenOffice.org table?
  </question>

  <\answer>
    \;

    <\enumerate>
      <item>Create a PostScript image of your table using OpenOffice
      (<samp|File<with|mode|math|\<rightarrow\>>Print<with|mode|math|\<rightarrow\>>Print
      to file)>. Name the file, for example, ``<kbd|table1.ps>''.

      <item>Convert the PostScript file to an Encapsulated PostScript file
      (<verbatim|.eps>) using <verbatim|ps2epsi> in a shell. Just type:
      ``<kbd|ps2epsi table1.ps table1.eps>''.

      <item>Insert or link the image <verbatim|table.eps> in the <TeXmacs>
      document.
    </enumerate>
  </answer>

  <section*|Microtypography>

  <\question>
    <label|typo-1>There is too much space after an abbreviation like
    <abbr|wrt.> or <abbr|etc.>?
  </question>

  <answer|This is because <TeXmacs> thinks, that the sentence ends after the
  dot in the abbreviation. To resolve this <em|a posteriori>, mark the
  abbreviation and press <key|A-a>. To care for this while writing: <key|A-a>
  <key|etc.> <key|[right]>.>

  <\question>
    <label|typo-2>How to add unbreakable space?
  </question>

  <answer|Type <key|M-/> after the space.>

  <section*|Remaining legacy questions>

  <\question>
    <label|legacy-1>A publisher sent me a giant <LaTeX> preamble I'm supposed
    to put in in order to prepare a book for them. What is the best way of
    putting it in and figuring if it will work?
  </question>

  <\answer>
    I recommand to convert the preamble to <TeXmacs> and to put the result in
    a <TeXmacs> style file. However, the result will probably be
    disappointing, because conversion between <TeX>/<LaTeX> and <TeXmacs> is
    not yet perfect and style files are particularly problematic. What you
    can also do is write a <TeXmacs> style file by your own which supports
    the major extra constructs you want to use from the editors style file.
    When you convert your book to <LaTeX>, you next use the editors style.
    Some layout will probably need to be redone at that stage, but this
    should actually be the work of the editor... Please look in the <TeXmacs>
    help for more information about convertions between <TeXmacs> and
    <LaTeX>.
  </answer>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|1.|?>>
    <associate|typo-1|<tuple|18|?>>
    <associate|usage-1|<tuple|15|?>>
    <associate|usage-2|<tuple|16|?>>
    <associate|typo-2|<tuple|19|?>>
    <associate|usage-3|<tuple|17|?>>
    <associate|general-1|<tuple|1|?>>
    <associate|general-2|<tuple|2|?>>
    <associate|general-3|<tuple|3|?>>
    <associate|general-4|<tuple|4|?>>
    <associate|general-5|<tuple|5|?>>
    <associate|general-6|<tuple|6|?>>
    <associate|general-7|<tuple|7|?>>
    <associate|cygwin-1|<tuple|12|?>>
    <associate|cygwin-2|<tuple|13|?>>
    <associate|cygwin-3|<tuple|14|?>>
    <associate|toc-10|<tuple|2.1|?>>
    <associate|sys-1|<tuple|8|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|toc-11|<tuple|2.2|?>>
    <associate|toc-12|<tuple|2.3|?>>
    <associate|sys-2|<tuple|9|?>>
    <associate|toc-13|<tuple|2.4|?>>
    <associate|sys-3|<tuple|10|?>>
    <associate|toc-14|<tuple|3|?>>
    <associate|sys-4|<tuple|11|?>>
    <associate|toc-15|<tuple|3.1|?>>
    <associate|toc-16|<tuple|3.2|?>>
    <associate|toc-17|<tuple|4|?>>
    <associate|toc-18|<tuple|4.1|?>>
    <associate|toc-19|<tuple|4.2|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|1.1|?>>
    <associate|toc-3|<tuple|1.2|?>>
    <associate|toc-4|<tuple|1.3|?>>
    <associate|toc-5|<tuple|1.4|?>>
    <associate|toc-6|<tuple|1.5|?>>
    <associate|legacy-1|<tuple|20|?>>
    <associate|toc-7|<tuple|1.6|?>>
    <associate|toc-8|<tuple|1.7|?>>
    <associate|toc-9|<tuple|2|?>>
  </collection>
</references>