<TeXmacs|2.1.1>

<style|<tuple|tmweb2|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Installing <TeXmacs> as an RPM
  package|<tmweb-download-links>>

  <section|The idea behind rpm packages>

  If you use <name|RedHat Linux> or another linux distribution which supports
  the <with|font-family|tt|rpm> command to install software, then it is very
  easy to install GNU <TeXmacs>. You can check whether
  <with|font-family|tt|rpm> exists on your system using

  <\shell-code>
    which rpm
  </shell-code>

  If the program is not installed, or if you do not have root privilege, then
  you should ask your system administrator to install GNU <TeXmacs>, or use
  the <hlink|classical installation method|unix.en.tm>.

  <section|Download, install and run>

  After <hlink|downloading|<merge|http://www.texmacs.org/Download/ftp/tmftp/redhat/|<merge|<TeXmacs-version-release|devel-release>|.i386.rpm>>>
  the GNU <TeXmacs> distribution for standard Intel or AMD based PC's under
  GNU/<name|Linux>, you may install the software from a terminal by typing
  (as root)

  <\shell-code>
    rpm -i <merge|<TeXmacs-version-release|devel-release>|.i386.rpm>
  </shell-code>

  The program can now be launched using

  <\shell-code>
    texmacs &
  </shell-code>

  <section|Happy <TeXmacs>-ing!>

  If you like the program, then please consider
  <hlink|donating|../contribute/donations.en.tm> money or services to us. Of
  course, you may also <hlink|contribute|../contribute/contribute.en.tm>
  yourself. In case of problems, please <hlink|subscribe|../home/ml.en.tm> to
  the <verbatim|texmacs-dev> or <verbatim|texmacs-users> mailing lists and
  ask your questions there. You may also directly
  <hlink|contact|../contact/contact.en.tm> us, but you might need to be more
  patient.

  <tmdoc-copyright|1999\U2011|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>