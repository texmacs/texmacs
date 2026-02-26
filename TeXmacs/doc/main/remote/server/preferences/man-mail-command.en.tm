<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Mail command>

  The mail command is a shell command that sends an email. It receives the
  message content via a temporary file. Available placeholders:

  <\description>
    <item*|$USER_PSEUDO>The user pseudo\M

    <item*|$USER_NAME>The user full name

    <item*|$USER_EMAIL>The user email address

    <item*|$USER_CODE>The confirmation or reset code

    <item*|$FILE_NAME>Path to the temporary file containing the message
  </description>

  Examples:

  <\verbatim-code>
    sendmail $USER_EMAIL \<less\> $FILE_NAME
  </verbatim-code>

  or

  <\verbatim-code>
    cat $FILE_NAME \| mail $USER_EMAIL -s \\"TeXmacs server Account
    information\\" -r 'reply-to@mydomain.com'
  </verbatim-code>

  <tmdoc-copyright|2025|Robin Wils>

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
    <associate|preamble|false>
  </collection>
</initial>