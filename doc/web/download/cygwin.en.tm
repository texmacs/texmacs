<TeXmacs|1.0.3.7>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Cygwin>|<tmweb-download-links>>

  There are basically two methods to get <TeXmacs> working under
  <name|Cygwin>:

  <\enumerate-numeric>
    <item>Installation of the official Cygwin package (recommended). Follow
    these <hlink|Instructions|http://www.fmi.uni-passau.de/~seidl/texmacs/tmtour.html#2>.

    <item>Download, compile and install <TeXmacs> manually. Follow the
    Instructions below, but note that the information about dependent
    packages is outdated; See the file <hlink|setup.hint|http://alice.fmi.uni-passau.de/~seidl/cygwin/release/TeXmacs/setup.hint>
    (from the abovementioned package) for the latest information.\ 
  </enumerate-numeric>

  <value|hrule>

  You may use <TeXmacs> on a Windows platform via <name|Cygwin>.
  <name|Cygwin> is a Unix distribution which can be used directly under
  Windows. In order to install <TeXmacs>, you should therefore first install
  <name|Cygwin> and next <TeXmacs>. This page describes how to do this by
  compiling <TeXmacs> from source.

  <section|Install <name|Cygwin> with <with|font-family|tt|setup.exe>>

  The last version of <name|Cygwin> can be installed from
  <hlink|<with|font-family|tt|http://www.cygwin.com>|http://www.cygwin.com>
  as follows:

  <\enumerate>
    <item>Download <hlink|<with|font-family|tt|setup.exe>|http://www.cygwin.com/setup.exe>
    and execute it.

    <item>Choose for instance to install via internet. Indicate the
    installation directory (e.g. <with|font-family|tt|C:\\cygwin>). If
    needed, indicate a proxy and the corresponding port. Also indicate a
    directory for the installation files (e.g.
    <with|font-family|tt|C:\\Cygwin-installation>). It can take a long time
    (30 min) to download everything.

    <item>It is probably simplest to perform a complete installation. For
    each non selected package, you may select it by click on
    <with|font-series|bold|Default> so that it becomes
    <with|font-series|bold|Install>. However, you should <em|not> install the
    <with|font-family|tt|ghostscript> package (you <em|should> install
    <with|font-family|tt|ghostscript-base> and
    <with|font-family|tt|ghostscript-x11>), by selecting
    <with|font-series|bold|Skip> for this package.

    If, for some reason, you do not want to perform a complete installation,
    then you should make sure that you install at least the following
    packages: <with|font-family|tt|gcc>, <with|font-family|tt|make>,
    <with|font-family|tt|tetex>, <with|font-family|tt|tetex-devel>,
    <with|font-family|tt|texmf>, <with|font-family|tt|XFree86-base>,
    <with|font-family|tt|XFree86-prog>, <with|font-family|tt|ghostscript-x11>
    and all <name|Guile> packages (<with|font-family|tt|guile>,
    <with|font-family|tt|guile-devel>, <with|font-family|tt|guile-doc>,
    <with|font-family|tt|libguile12> and <with|font-family|tt|libguile14>).
    You probably also want a decent window manager like
    <with|font-family|tt|openbox> or <with|font-family|tt|WindowMaker>.

    <item>Install.

    <item>Put <with|font-family|tt|c:\\cygwin> at the start of the
    <with|font-family|tt|PATH> environment variable and
    <with|font-family|tt|127.0.0.1:0.0> for <with|font-family|tt|DISPLAY> in
    the menu <with|font-series|bold|Start-\<gtr\>Control
    Panel-\<gtr\>System-\<gtr\>Advanced-\<gtr\>Environment variables>.

    <item>In order to test your installation, you may

    <\itemize>
      <item>Launch <name|Cygwin> from your desktop using the link.

      <item>In the <name|Cygwin> window, launch the X server using
      <with|font-family|tt|XWin&> and the window manager using
      <with|font-family|tt|fvwm2&>.

      <item>In the X window, try launching an Xterm using the popup menu when
      pressing the right-hand button.
    </itemize>
  </enumerate>

  <section|Download the <TeXmacs> distribution>

  Download the <hlink|latest version (2.5
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<extern|texmacs-version-release|tgz>|-src.tar.gz>>>
  of the source code, or the <hlink|latest stable version (2.4
  Mb)|<merge|ftp://ftp.texmacs.org/pub/TeXmacs/targz/|<merge|<extern|texmacs-version-release|stgz>|-src.tar.gz>>>.
  You may for instance save the distribution in
  <with|font-family|tt|C:\\tmp>. The name of the file with the distribution
  is of the form <with|font-family|tt|TeXmacs-[source version]-src.tar.gz>,
  where <with|font-family|tt|[source version]> is the corresponding version.

  <section|Compile <TeXmacs>>

  Launch an Xterm as explained at the end of step 1 and, assuming that you
  saved the <TeXmacs> distribution in <with|font-family|tt|C:\\tmp>, type the
  following commands in your Xterm:

  <\code>
    \ \ \ \ cd c:/tmp

    \ \ \ \ tar -zxvf TeXmacs-[source version]-src.tar.gz

    \ \ \ \ cd TeXmacs-[source version]-src

    \ \ \ \ ./configure

    \ \ \ \ make

    \ \ \ \ make install
  </code>

  You should now be able to run <TeXmacs> using

  <\code>
    \ \ \ \ texmacs &
  </code>

  <section|Finishing touches>

  <\itemize>
    <item>Unless you have a a US keyboard you will nead a keyboard mapping
    file for your keyboard layout. I found the one I use (swedish/finnish) on
    the page

    <\verbatim>
      \ \ \ \ <hlink|<with|font-family|tt|http://www-user.tu-chemnitz.de/~goal/>|http://www-user.tu-chemnitz.de/~goal/>
    </verbatim>

    The file is a text-file that should be named
    <with|font-family|tt|.xmodmap>, rename it to that if it's called
    something else (This won't work in Windows, use the <name|Cygwin> command
    <with|font-family|tt|mv> instead, like

    <\verbatim>
      \ \ \ \ mv <em|old_name> .xmodmap
    </verbatim>

    The file should be placed in your <name|Cygwin> home directory (for me
    <with|font-family|tt|/usr/home>, i.e.
    <with|font-family|tt|C:\\cygwin\\usr\\home>).

    A French user downloaded

    <\verbatim>
      \ \ \ \ <hlink|<with|font-family|tt|http://www-user.tu-chemnitz.de/~goal/xfree/XF86Config-4.bz2>|http://www-user.tu-chemnitz.de/~goal/xfree/XF86Config-4.bz2>
    </verbatim>

    and he had to set the <with|font-family|tt|XbmModel> to
    <with|font-family|tt|pc105> and the <with|font-family|tt|XkbLayout> to
    <with|font-family|tt|fr>.

    <item>If you want <TeXmacs> to correctly display images of most of the
    standard formats, then you should install the
    <with|font-family|tt|netpbm> library:

    <\enumerate>
      <item>Download the file <with|font-family|tt|netpbm-9.12-1-cygwin.tar.bz2>
      from

      <\verbatim>
        \ \ \ \ <hlink|<with|font-family|tt|http://netpbm.sourceforge.net>|http://netpbm.sourceforge.net>
      </verbatim>

      and save it into <with|font-family|tt|C:\\Cygwin-installation>.

      <item>In your Xterm, type

      <\code>
        \ \ \ \ cd c:/cygwin

        \ \ \ \ tar -jxvf /cygdrive/c/Cygwin-installation/netpbm-9.12-1-cygwin.tar.bz2
      </code>
    </enumerate>

    <item>If you want to use <TeXmacs> for browsing the web, then don't
    forget to put the following lines in your personal
    <with|font-family|tt|$HOME/.bash_profile>:

    <\code>
      \ \ \ \ export http_proxy=http://proxy:port

      \ \ \ \ export ftp_proxy=http://proxy:port
    </code>

    where <with|font-family|tt|proxy> is the name of your proxy and
    <with|font-family|tt|port> the corresponding port.
  </itemize>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
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
  </collection>
</initial>