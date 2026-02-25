<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Notes and troubleshooting><label|sec-man-remote-notes>

  <subsection|Versioning vs. live documents: choosing the right tool>

  A common point of confusion is the difference between <hlink|<TeXmacs>
  versioning system|../editing/man-versioning.en.tm> and live documents. Both
  support collaboration, but they serve different purposes and use different
  workflows.

  <\padded>
    <\wide-tabular>
      <tformat|<cwith|1|1|1|-1|cell-background|pastel
      grey>|<cwith|1|-1|2|2|cell-rborder|1ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|-1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bsep|1spc>|<cwith|1|-1|1|-1|cell-tsep|1spc>|<cwith|1|-1|1|-1|cell-lsep|2spc>|<cwith|1|-1|1|-1|cell-rsep|2spc>|<cwith|1|1|1|1|cell-tborder|1ln>|<cwith|2|2|1|1|cell-bborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|1|cell-rborder|1ln>|<cwith|1|-1|2|2|cell-lborder|1ln>|<table|<row|<\cell>
        <strong|Versioning system (for remote files)>
      </cell>|<\cell>
        <strong|Live documents>
      </cell>>|<row|<\cell>
        <em|Use when you want:>

        <with|item-hsep|1.5tab|item-vsep|0fn|<\itemize-dot>
          <item>Asynchronous collaboration

          <item>Complete control over accepting or rejecting changes

          <item>A permanent record of all versions and who made which changes

          <item>The ability to compare versions side-by-side and selectively
          merge

          <item>Integration with external version control systems like Git or
          Subversion

          <item>To review changes before incorporating them into your
          document
        </itemize-dot>>
      </cell>|<\cell>
        <em|Use when you want:>

        <with|item-hsep|1.5tab|item-vsep|0spc|<\itemize-dot>
          <item>Real-time collaboration

          <item>Immediate feedback and visibility of changes

          <item>Natural back-and-forth during meetings or working sessions

          <item>Patch-based merging of compatible edits

          <item>Faster iteration and dynamic collaboration
        </itemize-dot>>
      </cell>>>>
    </wide-tabular>
  </padded>

  In practice, you can use both approaches: live documents for active working
  sessions and brainstorming, and remote files with versioning for more
  formal document management and long-term storage.

  <subsection|Security and privacy>

  When using remote tools, keep in mind:

  <\itemize>
    <item>All communication between client and server can be encrypted using
    TLS if the server supports it.

    <item>Passwords are stored using salted hashing on the server.

    <item>Access control for resources (files, directories, chat rooms) can
    be configured by resource owners.

    <item>Server administrators may have access to all stored content for
    backup and maintenance purposes.

    <item>Check with your server administrator about data retention policies
    and backup procedures.
  </itemize>

  <subsection|Troubleshooting>

  If you encounter connection issues:

  <\itemize>
    <item>Verify that you entered the correct server address and port number.

    <item>Check that your network allows outgoing connections to the server.

    <item>Ensure your username and password are correct.

    <item>If the server uses TLS, verify that the GnuTLS plugin is properly
    installed.

    <item>Contact your server administrator if problems persist.
  </itemize>

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