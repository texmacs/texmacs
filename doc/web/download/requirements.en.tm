<TeXmacs|1.0.5.9>

<style|tmweb>

<\body>
  <tmweb-current|Download|Requirements><tmweb-title|System requirements for
  running GNU <TeXmacs>|<tmweb-download-links>>

  <TeXmacs> currently runs on most <name|Unix> systems, including
  <name|Windows> (using <name|Cygwin>) and <name|MacOS-X> (using
  <name|Fink>). A native <name|Windows> port is also available. We recommend
  running <TeXmacs> on fast computers in order to obtain a maximal
  reactivity. An absolute minimal configuration is a 200MHz processor and
  32Mb of memory, but \<gtr\>1GHz and \<gtr\>128Mb are recommended.

  In order to run <TeXmacs>, it is recommended to have a
  <hyper-link|<TeX>/<LaTeX>|tex-dep.en.tm> distribution on your system. Most
  modern <name|Linux> systems automatically come with such a system. If you
  want to compile <TeXmacs> yourself, then you should also have the
  <hyper-link|<name|Guile>/<name|Scheme>|guile-dep.en.tm> extension language
  installed. Below, we shortly describe how to install <TeX> and
  <name|Guile>.

  <section|Installation of a <TeX> system><label|TeX>

  The installation of a functional <TeX> distribution on your system can be a
  complicated job. We recommend you to install Te<TeX>, which has been most
  extensively tested in combination with <TeXmacs>.

  If you use redhat linux or another linux distribution (like mandrake) which
  supports the <with|font-family|tt|rpm> command, then Te<TeX> is most easily
  installed using this facility. You should first
  <hlink|download|http://at.rpmfind.net/opsys/linux/RPM/tetex.html> the
  Te<TeX> distribution which corresponds to your platform. An rpm package
  <with|font-family|tt|[package]-[version].rpm> is installed by root using

  <\code>
    \ \ \ \ rpm -i [package]-[version].rpm
  </code>

  An rpm package may depend on other rpm packages. When installing a package,
  you will be informed on missing dependencies. In our case you will probably
  need to install the <hlink|Te<TeX> fonts|http://at.rpmfind.net/opsys/linux/RPM/tetex-fonts.html>
  too. In order to know whether a particular package
  <with|font-family|tt|[package]-[version].rpm> has been installed on your
  system, you should type

  <\code>
    \ \ \ \ rpm -q [package]
  </code>

  In particular, before installing Te<TeX>, you should try

  <\code>
    \ \ \ \ rpm -q tetex
  </code>

  in order to know whether the package is already there on your system.

  If you do not have the <with|font-family|tt|rpm> program, then you may
  download it from

  <\verbatim>
    \ \ \ \ <hlink|ftp://ftp.rpm.org/pub/rpm/dist/rpm-3.0.x|ftp://ftp.rpm.org/pub/rpm/dist/rpm-3.0.x/rpm-3.0.4.i386.tar.gz>
  </verbatim>

  and unpack it in the <with|font-family|tt|/> directory as root. You may
  also <hlink|download|ftp://www.dante.de/tex-archive/systems/unix/teTeX/current/distrib/INSTALL>
  Te<TeX> from <hlink|CTAN|http://www.ctan.org/> by following the
  instructions given there. Finally, instead of installing a complete <TeX>
  distribution, it is also possible to directly install a tarball with the
  <hlink|most important fonts|fonts.en.tm#fonts-complete> provided by such a
  system.

  <section|Installation of guile><label|Guile>

  The easiest way to install <name|Guile> is again by downloading the rpm
  packages <hlink|guile|http://at.rpmfind.net/opsys/linux/RPM/guile.html> and
  <hlink|guile-devel|http://at.rpmfind.net/opsys/linux/RPM/guile-devel.html>
  which correspond to your platform and install them in a similar way as
  above. Otherwise, you may download the source code of the official
  <name|Guile> distribution from <hlink|ftp://ftp.gnu.org/pub/gnu/guile|ftp://ftp.gnu.org/pub/gnu/guile/guile-1.3.4.tar.gz>
  and follow the instructions below.

  If you have the permission to log yourself as root, then it is preferable
  that you log yourself as root and install the package by

  <\code>
    \ \ \ \ gunzip -c guile-1.3.4.tar.gz \| tar xvf -

    \ \ \ \ cd guile-1.3.4

    \ \ \ \ ./configure

    \ \ \ \ make

    \ \ \ \ make install
  </code>

  This will install <name|Guile> into <with|font-family|tt|/usr/local/share/guile>.
  The <name|Guile> binaries and libraries will be installed in
  <with|font-family|tt|/usr/local/bin> and
  <with|font-family|tt|/usr/local/lib> respectively. You should set the paths
  <with|font-family|tt|GUILE_LOAD_PATH>, <with|font-family|tt|PATH> and
  <with|font-family|tt|LD_LIBRARY_PATH> accordingly, using

  <\code>
    \ \ \ \ export GUILE_LOAD_PATH=/usr/local/share/guile/1.3.4

    \ \ \ \ export PATH=/usr/local/bin:$PATH

    \ \ \ \ export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
  </code>

  or

  <\code>
    \ \ \ \ setenv GUILE_LOAD_PATH /usr/local/share/guile/1.3.4

    \ \ \ \ setenv PATH /usr/local/bin:$PATH

    \ \ \ \ setenv LD_LIBRARY_PATH /usr/local/lib:$LD_LIBRARY_PATH
  </code>

  as a function of your shell.

  If you do not have the possibility to log yourself as root, and if your
  system administrator is not willing to install <name|Guile> in the above
  way, then you may also install <name|Guile> in another directory
  <with|font-family|tt|[dir]> as follows.

  <\code>
    \ \ \ \ gunzip -c guile-1.3.4.tar.gz \| tar xvf -

    \ \ \ \ cd guile-1.3.4

    \ \ \ \ ./configure --prefix=[dir]

    \ \ \ \ make

    \ \ \ \ make install
  </code>

  In this case, <name|Guile> will be installed in the directory
  <with|font-family|tt|[dir]>. The <name|Guile> binaries resp. libraries can
  be found in the subdirectories <with|font-family|tt|[dir]/bin> resp.
  <with|font-family|tt|[dir]/lib>. You should set the paths
  <with|font-family|tt|GUILE_LOAD_PATH>, <with|font-family|tt|PATH> and
  <with|font-family|tt|LD_LIBRARY_PATH> accordingly, using

  <\code>
    \ \ \ \ export GUILE_LOAD_PATH=[dir]/share/guile/1.3.4

    \ \ \ \ export PATH=[dir]/bin:$PATH

    \ \ \ \ export LD_LIBRARY_PATH=[dir]/lib:$LD_LIBRARY_PATH
  </code>

  or

  <\code>
    \ \ \ \ setenv GUILE_LOAD_PATH [dir]/share/guile/1.3.4

    \ \ \ \ setenv PATH [dir]/bin:$PATH

    \ \ \ \ setenv LD_LIBRARY_PATH [dir]/lib:$LD_LIBRARY_PATH
  </code>

  as a function of your shell.

  You may also download <name|Guile> from its official <hlink|home
  page|http://www.gnu.org/software/guile/guile.html>. You will also find more
  documentation about <name|Guile> there.

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>