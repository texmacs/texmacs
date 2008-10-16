<TeXmacs|1.0.6.9>

<style|tmweb>

<\body>
  <tmweb-current|Download|Binaries><tmweb-title|<TeXmacs> inside
  GNU/<name|Linux> distributions|<tmweb-download-links>>

  The following GNU/Linux distributions provide packages or third-part
  packages for <TeXmacs>:

  <\itemize>
    <item><hlink|Alt Linux|#alt-linux>.

    <item><hlink|Debian GNU/Linux|#debian>.

    <item><hlink|Fedora|#fedora>.

    <item><hlink|Gentoo|#gentoo>.

    <item><hlink|Knoppix|knoppix.en.tm>.

    <item><hlink|Mandriva|#mandriva>.

    <item><hlink|Open Suse|#suse>.

    <item><hlink|Red Hat|#redhat>.

    <item><hlink|Slackware|#slackware>.

    <item><hlink|Ubuntu|#ubuntu>.
  </itemize>

  Please report any other distributions with sufficiently up-to-date
  <TeXmacs> packages to us. The <hlink|up-to-date
  list|http://www.inp.nsk.su/~grozin/TeXmacs/> of distributions with
  <TeXmacs> is maintained by <hlink|<name|Andrey
  Grozin>|http://www.inp.nsk.su/~grozin/>.

  <section*|Alt Linux><label|alt-linux>

  Up-to-date TeXmacs rpms can be found in the <hlink|Sisyphus
  repository|ftp://ftp.altlinux.ru/pub/distributions/ALTLinux/Sisyphus/>.

  <section*|Debian><label|debian>

  <TeXmacs> is available in <hlink|<name|Debian>
  <name|GNU>/<name|Linux>|http://www.debian.org>. On a correctly configured
  <name|Debian> system, and with net access, all you have to do to install
  <TeXmacs> (as <verbatim|root>) is:

  <verbatim| \ \ \ apt-get install texmacs>

  As usual, you may obtain further information about the <TeXmacs> package
  (dependencies on other packages etc.) by executing the command

  <verbatim| \ \ \ apt-cache show texmacs>

  Alternatively, you may obtain the package information and the package
  itself from the following web pages:

  <\itemize>
    <item><hlink|Debian package for the stable
    distribution|http://packages.debian.org/stable/editors/texmacs.html>.

    <item><hlink|Debian package for the testing
    distribution|http://packages.debian.org/testing/editors/texmacs.html>.

    <item><hlink|Debian package for the unstable
    distribution|http://packages.debian.org/unstable/editors/texmacs.html>.

    <item><hlink|Debian package for the experimental
    distribution|http://packages.debian.org/experimental/editors/texmacs>.
  </itemize>

  If you are running the "<verbatim|unstable>" distribution, you can get
  <TeXmacs> from "<verbatim|experimental>" (by including the corresponding
  line in your <verbatim|/etc/apt/sources.list>) which is rather up-to-date.
  Versions in "<verbatim|stable>", "<verbatim|testing>", and
  "<verbatim|unstable>" are way too old. The package is currently maintained
  by Atsuhito Kohda and Kamaraju Kusumanchi.

  <section*|Fedora><label|fedora>

  <TeXmacs> lives in <hlink|Fedora extras|http://fedoraproject.org/wiki/Extras/>.
  Just do

  <verbatim| \ \ \ yum install TeXmacs>

  Later, the command

  <verbatim| \ \ \ yum update>

  will update all installed packages, including <TeXmacs>.

  <section*|Gentoo><label|gentoo>

  The portage tree of <hlink|Gentoo|http://www.gentoo.org/> usually contains
  an outdated version of <TeXmacs>. An up-to-date version can be found in the
  <hlink|Gentoo science overlay|http://gentooscience.org/>.

  First time installation:

  <\enumerate>
    <item><verbatim|emerge layman>

    <item><verbatim|layman -a science>

    <item>Add the line: <verbatim|source /usr/portage/local/layman/make.conf>
    to your <verbatim|/etc/make.conf>

    <item><verbatim|emerge layman>

    <item>Add the line: <verbatim|app-office/texmacs ~x86> to your
    <verbatim|/etc/portage/packages.keywords> (of course, assuming your
    computer is x86)

    <item><verbatim|emerge texmacs>
  </enumerate>

  Updating <TeXmacs>

  <\enumerate>
    <item><verbatim|layman -s science>

    <item><verbatim|emerge texmacs>
  </enumerate>

  <section*|Mandriva><label|mandriva>

  Recent versions of <hlink|Mandriva|http://www.mandriva.com/> contain rather
  up-to-date <TeXmacs> in <verbatim|contrib>.

  <section*|Open Suse><label|suse>

  Recent versions of <hlink|OpenSUSE|http://www.opensuse.org/> contain rather
  up-to-date <TeXmacs>. Just use <verbatim|yast> to install it.

  <section*|Red Hat><label|redhat>

  Users of <hlink|Red Hat Enterprise Linux|http://www.redhat.com/rhel/> and
  its clones (<hlink|CentOS|http://www.centos.org/>, <hlink|CERN Scientific
  Linux|http://linux.web.cern.ch/linux/scientific4/>, <hlink|Oracle
  Unbreakable Linux|http://www.oracle.com/Linux/>, etc.) can find rpms of
  recent versions of <TeXmacs> at <hlink|Dag Vieers
  repository|http://dag.wieers.com/rpm/>. Just install the
  <hlink|rpmforge-release|http://dag.wieers.com/rpm/packages/rpmforge-release/>
  package for your distribution, it contains all the necessary config files.
  Then

  <verbatim| \ \ \ yum install texmacs>

  As usual,

  <verbatim| \ \ \ yum update>

  will update all installed packages, including <TeXmacs>.

  <section*|Slackware><label|slackware>

  If you go to <hlink|Italian Slackware Community|http://www.slacky.it/> and
  search for <TeXmacs>, you will find an up-to-date package. The site is
  mostly in Italian.

  <section*|Ubuntu><label|ubuntu>

  <hlink|Ubuntu|http://www.ubuntu.com/> is based on <hlink|Debian|#debian>
  "<verbatim|unstable>", and therefore contains <TeXmacs>. You can (probably)
  install an up-to-date TeXmacs from Debian "<verbatim|experimental>".

  <tmdoc-copyright|1999--2007|Andrey Grozin|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>