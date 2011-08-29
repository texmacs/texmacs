<TeXmacs|1.0.7.11>

<style|tmdoc>

<\body>
  <tmdoc-title|The <name|Mycas> plug-in>

  The <name|Mycas> plug-in is a very simple example of how you might write
  your own plug-in. In order to try it, compile the program
  <verbatim|mycas.cpp> in the <verbatim|src> subdirectory using

  <\shell-code>
    g++ mycas.cpp -o mycas
  </shell-code>

  and move the binary to some place in your path. When starting <TeXmacs>,
  you will see a <menu|Mycas> entry in the <menu|Insert|Session> menu. Below,
  we briefly explain how the <name|Mycas> plug-in works. For more details on
  how to write your own plug-ins, look at <menu|Help|Interfacing>. A lot of
  additional examples are provided in <verbatim|$TEXMACS_PATH/examples/plugins>.

  <section|Studying the source code step by step>

  Let us study the source code of <verbatim|mycas.cpp> step by step. First,
  all communication takes place via standard input and output, using pipes.
  In order to make it possible for <TeXmacs> to know when the output from
  your system has finished, all output needs to be encapsulated in blocks,
  using three special control characters:

  <\cpp-code>
    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)
  </cpp-code>

  <\verbatim>
    \ \ \ \ 
  </verbatim>

  The <verbatim|DATA_ESCAPE> character followed by any other character
  <math|c> may be used to produce <math|c>, even if <math|c> is one of the
  three control characters. An illustration of how to use
  <verbatim|DATA_BEGIN> and <verbatim|DATA_END> is given by the startup
  banner:

  <\cpp-code>
    <\verbatim>
      int

      main () {

      \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

      \ \ cout \<less\>\<less\> "----------------------------------------------------\\n";

      \ \ cout \<less\>\<less\> "Welcome to my test cas for TeXmacs\\n";

      \ \ cout \<less\>\<less\> "This software comes with no warranty
      whatsoever\\n";

      \ \ cout \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";

      \ \ cout \<less\>\<less\> "----------------------------------------------------\\n";

      \ \ next_input ();

      \ \ cout \<less\>\<less\> DATA_END;

      \ \ fflush (stdout);
    </verbatim>
  </cpp-code>

  The first line of <verbatim|main> says that the startup banner will be
  printed in the ``verbatim'' format. The <verbatim|next_input> function,
  which is called after outputting the banner, is used for printing a prompt
  and will be detailed later. The final <verbatim|DATA_END> closes the
  startup banner block and tells <TeXmacs> that <verbatim|mycas> is waiting
  for input. Don't forget to flush the standard output, so that <TeXmacs>
  will receive the whole message.

  The main loop starts by asking for input from the standard input:

  <\cpp-code>
    \ \ while (1) {

    \ \ \ \ char buffer[100];

    \ \ \ \ cin \<gtr\>\<gtr\> buffer;

    \ \ \ \ if (strcmp (buffer, "quit") == 0) break;
  </cpp-code>

  <\verbatim>
    \ \ \ \ \ \ 
  </verbatim>

  The output which is send back should again be enclosed in a
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block.

  <\cpp-code>
    <\verbatim>
      \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

      \ \ \ \ cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer
      \<less\>\<less\> "\\n";
    </verbatim>
  </cpp-code>

  Inside such a block you may recursively send other blocks, which may be
  specified in different formats. For instance, the following code will send
  a <LaTeX> formula:

  <\cpp-code>
    <\verbatim>
      \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula: ";

      \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:"
      \<less\>\<less\> "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;

      \ \ \ \ cout \<less\>\<less\> "\\n";
    </verbatim>
  </cpp-code>

  For certain purposes, it may be useful to directly send output in <TeXmacs>
  format using a <scheme> representation:

  <\cpp-code>
    <\verbatim>
      \ \ \ \ cout \<less\>\<less\> "And finally a fraction ";

      \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:"

      \ \ \ \ \ \ \ \ \ \<less\>\<less\> "(frac \\"a\\" \\"b\\")"
      \<less\>\<less\> DATA_END;

      \ \ \ \ cout \<less\>\<less\> ".\\n";
    </verbatim>
  </cpp-code>

  \;

  In order to finish, we should again output the matching <verbatim|DATA_END>
  and flush the standard output:

  <\cpp-code>
    <\verbatim>
      \ \ \ \ next_input ();

      \ \ \ \ cout \<less\>\<less\> DATA_END;

      \ \ \ \ fflush (stdout);

      \ \ }

      \ \ return 0;

      }
    </verbatim>
  </cpp-code>

  \;

  Notice that you should never output more than one
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block. As soon as the first
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block has been received by
  <TeXmacs>, it is assumed that your system is waiting for input. If you want
  to send several <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks, then they
  should be enclosed in one main block.

  A special ``channel'' is used in order to send the input prompt. Channels
  are specified as special <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks:

  <\cpp-code>
    <\verbatim>
      static int counter= 0;

      \;

      void

      next_input () {

      \ \ counter++;

      \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "channel:prompt"
      \<less\>\<less\> DATA_END;

      \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
      \<less\>\<less\> "] ";

      }
    </verbatim>
  </cpp-code>

  \;

  Inside the prompt channel, you may again use
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks in a nested way. This
  allows you for instance to use a formula as a prompt. There are three
  standard channels:

  <\description>
    <item*|<verbatim|output>>The default channel for normal output.

    <item*|<verbatim|prompt>>For sending input prompts.

    <item*|<verbatim|input>>For specifying a default value for the next
    input.
  </description>

  <section|Graphical output>

  It is possible to send postscript graphics as output. Assume for instance
  that you have a picture <verbatim|picture.ps> in your home directory. Then
  inserting the lines:

  <\cpp-code>
    <\verbatim>
      \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";

      \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "ps:";

      \ \ \ \ fflush (stdout);

      \ \ \ \ system ("cat $HOME/picture.ps");

      \ \ \ \ cout \<less\>\<less\> DATA_END;

      \ \ \ \ cout \<less\>\<less\> "\\n";
    </verbatim>
  </cpp-code>

  at the appropriate place in the main loop will display your image in the
  middle of the output.

  <tmdoc-copyright|2011|Joris van der Hoeven>

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
  </collection>
</initial>