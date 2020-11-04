<TeXmacs|1.99.9>

<style|<tuple|tmweb|english|old-spacing>>

<\body>
  <tmdoc-title|Frequently asked questions>

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
      <item><hlink|How can I see the <LaTeX> or <TeX> code corresponding to
      what I see on the screen|#sys-2>?

      <item><hlink|Trying to start <TeXmacs> remotely over a ssh connection,
      I get an error, while <abbr|e.g.> xclock works without a
      problem|#sys-4>?

      <item><hlink|<TeXmacs> is unusually slow while editing|#sys-5>?
    </itemize-minus>

    <item>Windows and Cygwin specific

    <\itemize-minus>
      <item><hlink|My Windows username contains spaces. Will this cause
      problems|#cygwin-4>?

      <item><hlink|What to do with Cygwin specific questions|#cygwin-1>?

      <item><hlink|How to associate .tm files with <TeXmacs> inside
      Cygwin|#cygwin-2>?

      <item><hlink|How to get spell checking working inside
      Cygwin|#cygwin-3>?
    </itemize-minus>

    <item>Usage

    <\itemize-minus>
      <item><hlink|How to mark/select a whole paragraph|#usage-1>?

      <item><hlink|Spell checking always starts at the beginning of the
      document|#usage-2>?

      <item><hlink|How can I insert an OpenOffice.org table|#usage-3>?

      <item><hlink|How can I insert the number of pages|#usage-4>?

      <item><hlink|How can I resize an image|#usage-5>?
    </itemize-minus>

    <item>Microtypography

    <\itemize-minus>
      <item><hlink|There is too much space after an abbreviation like
      <abbr|wrt.> or <abbr|etc.>|#typo-1>?

      <item><hlink|How to add unbreakable space|#typo-2>?
    </itemize-minus>

    <item>Legacy Questions

    <\itemize-minus>
      <item><hlink|A publisher sent me a giant <LaTeX> preamble I'm supposed
      to put in in order to prepare a book for them. What is the best way of
      putting it in and figuring if it will work|#legacy-1>?
    </itemize-minus>
  </itemize>

  <section*|General>

  <\question>
    <label|general-1>What is <TeXmacs>?
  </question>

  <\answer*>
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
      very much appreciate your <hlink|help|https://www.texmacs.org/Web/Contribute.html>
      for writing and improving converters for <TeXmacs> documents.
    </itemize-dot>
  </answer*>

  <\question>
    <label|general-2>For what typical purposes do people use <TeXmacs>?
  </question>

  <\answer*>
    <TeXmacs> can be used for

    <\itemize>
      <item>Books and Articles. They can be written fully within <TeXmacs>.
      If your publisher requires a certain <LaTeX> style for an article, then
      as a last step you can export your document to <LaTeX> and make the
      final modifications there.

      <item>Presentations.

      <item>Interface to computer algebra systems and other scientific
      software.

      <item>Webpages. For example the <TeXmacs> webpage is maintained as
      <TeXmacs> documents, which are exported to <name|Html>.
    </itemize>
  </answer*>

  <\question>
    <label|general-3>What Operation Systems are supported?
  </question>

  <\answer*>
    <TeXmacs> can be easily installed on all major systems, such as
    GNU/<name|Linux>, <name|MacOS> and <name|Windows>. There are <name|rpm>
    and Debian packages as well as binaries available for <name|Linux>. For
    Mac <name|OS X> there is a <name|Fink> package. For <name|MS Windows>, a
    <name|Cygwin> package is available.\ 
  </answer*>

  <\question>
    <label|general-4>What are the system requirements?
  </question>

  <answer*|A reasonably fast machine is recommended. Having said this, I
  (Andreas) am typing this on a Pentium III 450Mhz under Cygwin, and
  <TeXmacs> is still useable here, although not overly reactive.>

  <\question>
    <label|general-5>I have already learned <TeX>/<LaTeX> and <name|Emacs>,
    do I have to learn all over again?
  </question>

  <answer*|You will find out, that much <LaTeX>-knowledge can be reused. For
  example, you can start a section by typing <key|\\section[return]> or you
  get <math|\<alpha\><rsub|1>> by typing <key|$\\alpha_1$>. Note that there
  are even shorter ways provided, for example <key|$a[tab]$> gives you an
  alpha as well, so you might want to migrate after a while. Styles like
  article, book or seminar, as known from <LaTeX>, are provided as well.
  Furthermore, many <name|Emacs> shortcuts like <key|C-x C-s> for saving a
  file, or <key|C-_> for undo will work. >

  <\question>
    <label|general-6>I have a question, where should I ask it?
  </question>

  <answer*|The best place is the <TeXmacs> user mailing list. Search this
  list and the FAQ beforehand, to ensure your question was not asked before.>

  <\question>
    <label|general-7>Is it free Software?
  </question>

  <answer*|<TeXmacs> falls under the GNU public licence. >

  <section*|System Appearance and Behavior>

  <\question>
    <label|sys-2>How can I see the <LaTeX> or <TeX> code corresponding to
    what I see on the screen?
  </question>

  <answer*|This question is due to a fundamental misunderstanding about
  <TeXmacs>. Indeed, <TeXmacs> is not <em|based> on <TeX>/<LaTeX>, although
  it does support (not yet perfect) <em|conversion> to and from <LaTeX>.
  Furthermore, in theory at least, there is actually no need anymore to look
  at something like the <TeX> source, since <TeXmacs> is guaranteed to be
  fully WYSIWYG. Conversion to <LaTeX> may only be useful, when transmitting
  an accepted paper to the publisher of a journal.>

  <\question>
    <label|sys-4>Trying to start <TeXmacs> remotely over a ssh connection, I
    get an error, while <abbr|e.g.> xclock works without a problem?
  </question>

  <\answer*>
    If you get an error message including the following:

    <verbatim|Fatal error: I failed to connect to Xserver in
    'x_display_rep::x_display_rep>

    then execute <verbatim|export DISPLAY=127.0.0.1:10.0> on the remote
    machine and make sure your /etc/hosts file is sound, <abbr|i.e.> it
    contains the line <verbatim|127.0.0.1 localhost> and the IP information
    about the local and remote machine.
  </answer*>

  <\question>
    <label|sys-5><TeXmacs> is unusually slow while editing?
  </question>

  <\answer*>
    Graphics card drivers with poor 2D performance can slow <TeXmacs> down
    tremendously.\ 

    If you are running TeXmacs under X on a system with an ATI graphics card
    and are using ATI's proprietary drivers (the driver called
    <verbatim|fglrx)>, you can achieve a significant speedup by

    <\itemize>
      <item><em|either> running an X server such as Xgl that performs 2D
      operations using the 3D API of the graphics card driver,

      <item><em|or> switching to an open source graphics card driver such as
      <verbatim|ati> or <verbatim|radeon> which have a far better 2D
      performance than <verbatim|fglrx>.
    </itemize>

    Note that if you try both at the same time, you will slow your system
    down even more, instead of speeding it up.
  </answer*>

  <section*|Windows and Cygwin specific>

  <\question>
    <label|cygwin-4>My Windows username contains spaces. Will this cause
    problems?
  </question>

  <answer*|It is strongly recommended to have a Windows username without
  spaces. Please make a new account, as simply changing the username could be
  insufficient.>

  <\question>
    <label|cygwin-1>What to do with Cygwin specific questions?
  </question>

  <\answer*>
    Look at the Cygwin FAQ at <hlink|http://cygwin.com/faq.html|http://cygwin.com/faq.html>,
    the Cygwin User Guide at <hlink|http://cygwin.com/cygwin-ug-net/|http://cygwin.com/cygwin-ug-net/>
    and search the Cygwin mailing list <hlink|http://www.cygwin.com/ml/cygwin/|http://www.cygwin.com/ml/cygwin/>.
  </answer*>

  <\question>
    <label|cygwin-2>How to associate .tm files with <TeXmacs> inside Cygwin?
  </question>

  <\answer*>
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
  </answer*>

  <\question>
    <label|cygwin-3>How to get spell checking working inside Cygwin?
  </question>

  <\answer*>
    Install the Cygwin package aspell. Execute in a shell:

    <\verbatim>
      \ \ \ \ cd /usr/bin<next-line> \ \ \ ln -s /usr/share/aspell/ispell
      ispell
    </verbatim>
  </answer*>

  <section*|Usage>

  <\question>
    <label|usage-1>How to mark/select a whole paragraph?
  </question>

  <answer*|Clicking once on a word will place the cursor there, clicking
  twice marks this word, clicking thrice will mark the paragraph and so on;
  eventually, the whole document will be selected.>

  <\question>
    <label|usage-2>Spell checking always starts at the beginning of the
    document?
  </question>

  <answer*|Mark a region, and spell checking will be restricted to this
  selection.>

  <\question>
    <label|usage-3>How can I insert an OpenOffice.org table?
  </question>

  <\answer*>
    \;

    <\enumerate>
      <item>Create a PostScript image of your table using OpenOffice
      (<samp|File<math|\<rightarrow\>>Print<math|\<rightarrow\>>Print to
      file)>. Name the file, for example, ``<kbd|table1.ps>''.

      <item>Convert the PostScript file to an Encapsulated PostScript file
      (<verbatim|.eps>) using <verbatim|ps2epsi> in a shell. Just type:
      ``<kbd|ps2epsi table1.ps table1.eps>''.

      <item>Insert or link the image <verbatim|table.eps> in the <TeXmacs>
      document.
    </enumerate>
  </answer*>

  <\question>
    <label|usage-4>How can I insert the number of pages?
  </question>

  <answer*|Put a label on the last page of your document and use the
  <verbatim|pageref> primitive in order to produce the corresponding number.>

  <\question>
    <label|usage-5>How can I resize an image?
  </question>

  <\answer*>
    Put your cursor right behind the image and edit the fields in the lowest
    icon bar. You may for instance set the width to <verbatim|5cm> and the
    height to <verbatim|27mm>. When leaving the width open, it will
    automatically be determined as a function of the height (and vice versa).
    When leaving both fields open, the image will be reproduced at its
    original size. Notice that a width of <verbatim|1par> will span your
    image over the paragraph width. The remaining two fields specify the
    <math|x>-offset and <math|y>-offset of the image. By default, images are
    aligned at the bottom. When specifying <verbatim|-0.5h> for the last
    field, the image will be vertically aligned at the center.
  </answer*>

  <section*|Microtypography>

  <\question>
    <label|typo-1>There is too much space after an abbreviation like
    <abbr|wrt.> or <abbr|etc.>?
  </question>

  <answer*|This is because <TeXmacs> thinks, that the sentence ends after the
  dot in the abbreviation. To resolve this <em|a posteriori>, mark the
  abbreviation and press <shortcut|(make 'abbr)>. To care for this while
  writing: <shortcut|(make 'abbr)><key*|e t c . right>.>

  <\question>
    <label|typo-2>How to add unbreakable space?
  </question>

  <answer*|Type <shortcut|(make 'no-break)> after the space.>

  <section*|Remaining legacy questions>

  <\question>
    <label|legacy-1>A publisher sent me a giant <LaTeX> preamble I'm supposed
    to put in in order to prepare a book for them. What is the best way of
    putting it in and figuring if it will work?
  </question>

  <\answer*>
    I recommend to convert the preamble to <TeXmacs> and to put the result in
    a <TeXmacs> style file. However, the result will probably be
    disappointing, because conversion between <TeX>/<LaTeX> and <TeXmacs> is
    not yet perfect and style files are particularly problematic. What you
    can also do is write a <TeXmacs> style file by your own which supports
    the major extra constructs you want to use from the editors style file.
    When you convert your book to <LaTeX>, you next use the editors style.
    Some layout will probably need to be redone at that stage, but this
    should actually be the work of the editor... Please look in the <TeXmacs>
    help for more information about conversions between <TeXmacs> and
    <LaTeX>.
  </answer*>

  <tmdoc-copyright|1998--2011|Andreas Seidl|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>