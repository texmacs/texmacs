<TeXmacs|1.99.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Users, groups, and permissions>

  The additional layer <scm|db-users.scm> allows you to specify read, write
  and administration permissions for database entries. There are two main
  types of users of <TeXmacs> databases: individual users and groups. Groups
  may delegate permissions to individual users or other groups. In addition,
  there are two special users <scm|"root"> (or <scm|#t>) and
  <scm|"anonymous"> (or <scm|#f>).

  <paragraph|Macros for context specification>

  <\explain>
    <scm|(with-user user . body)><explain-synopsis|specify current user>
  <|explain>
    Evaluate the <scm|body> using the permissions of the specified
    <scm|user>.
  </explain>

  <paragraph|Special attributes>

  <\description>
    <item*|<scm|pseudo>>Specifies a pseudo for the user. Notice that
    <scm|name> should specify the full name.

    <item*|<scm|owner>, <scm|readable>, <scm|writable>>Ownership and
    read/write permissions for an entry.

    <item*|<scm|delegate-owner>, <scm|delegate-readable>,
    <scm|delegate-writable>>Delegate group permissions.
  </description>

  <paragraph|Affected routines of the database API>

  <\explain>
    <scm|(db-set-field id attr vals)><explain-synopsis|set values for a given
    field>

    <scm|(db-set-entry id l)><explain-synopsis|fill out a complete entry>

    <scm|(db-remove-entry id)><explain-synopsis|remove a complete entry>
  <|explain>
    The actions only succeed when the current user has the appropriate
    permissions.
  </explain>

  <\explain>
    <scm|(db-get-field id attr)><explain-synopsis|get all values for a given
    field>

    <scm|(db-get-entry id)><explain-synopsis|retrieve a complete entry>

    <scm|(db-search q)><explain-synopsis|search for a list of fields>
  <|explain>
    Only values are returned for which the current user has appropriate
    permissions.
  </explain>

  <paragraph|Other useful routines>

  <\explain>
    <scm|(db-allow? id uid permission-attr)><explain-synopsis|check
    permissions>
  <|explain>
    Check whether a user with identifier <scm|uid> has a given permission
    <scm|permission-attr> for the entry with identifier <scm|id>.
  </explain>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>