<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|User preferences>

  Preferences are used to store any information you need to keep across
  different runs of <TeXmacs>, like window position and size, active menu
  bars, etc. Internally they are stored in the users home directory as a
  <scheme> list of items like <scm|("name" value)> which therefore has in
  principle no structure. However, a good practice to avoid conflicts is to
  prefix your options by the name of the plugin or module you are creating,
  like in <scm|"gui:help-window-position">.

  The first step in defining a new preference is adding it with
  <scm|define-preferences> and assigning a call-back function to handle
  changes in the preference. This is important for instance in menus, where a
  click on an item simply sets some preference to some value and it's up to
  the call-back to actually take the necessary actions.

  <\warning*>
    One may not store the boolean values <scm|#t>, <scm|#f> directly into
    preferences. Instead one should use the strings <scm|"on"> and
    <scm|"off">. This is due to the internal storage of default values for
    preferences using <scm|ahash-table>.
  </warning*>

  <\explain>
    <scm|(define-preferences <scm-arg|list>)><explain-synopsis|define new
    preferences with defaults and call-backs>
  <|explain>
    Each element of <scm-arg|list> is of the form <scm|("somename"
    default-value notify-procedure)> where <scm|notify-procedure> is a
    procedure taking two arguments like this:

    <scm|(define (notify-procedure property-name value) (do-things))>

    Remember to use the strings <scm|"on"> and <scm|"off"> instead of
    booleans <scm|#t>, <scm|#f>.

    <\unfolded-documentation>
      Example
    <|unfolded-documentation>
      <\session|scheme|default>
        <\input|Scheme] >
          (define (notify-test pref value)

          \ \ (display* "Hey! " pref " changed to " value) (newline))
        </input>

        <\input|Scheme] >
          (define-preferences ("test:pref" "off" notify-test))
        </input>

        <\unfolded-io|Scheme] >
          (get-preference "test:pref")
        <|unfolded-io>
          "off"
        </unfolded-io>

        <\input|Scheme] >
          (set-preference "test:pref" "on")
        </input>

        <\unfolded-io|Scheme] >
          (preference-on? "test:pref")
        <|unfolded-io>
          #t
        </unfolded-io>

        <\input|Scheme] >
          \;
        </input>
      </session>
    </unfolded-documentation>
  </explain>

  <\explain>
    <scm|(set-preference <scm-arg|name> <scm-arg|value>)><explain-synopsis|set
    user preference>
  <|explain>
    Save preference <scm|name> with value <scm|value>. Then call the
    call-back associated to this preference, as defined in
    <scm|define-preferences>.

    Remember to use the strings <scm|"on"> and <scm|"off"> instead of
    booleans <scm|#t>, <scm|#f>.
  </explain>

  <\explain>
    <scm|(append-preference <scm-arg|name>
    <scm-arg|value>)><explain-synopsis|appends a value to the list for a
    preference>
  <|explain>
    This convenience function appends <scm|value> to the list of values of
    preference <scm|name>, or creates a list with one element in case the
    preference didn't exist. The call-back associated to this preference, as
    defined in <scm|define-preferences> is called once the modification is
    done.
  </explain>

  <\explain>
    <scm|(reset-preference <scm-arg|name>)><explain-synopsis|delete user
    preference>
  <|explain>
    Deletes preference <scm|name> from the user preferences.
  </explain>

  <\explain>
    <scm|(get-preference <scm-arg|name>)><explain-synopsis|get user
    preference>
  <|explain>
    Returns the value of preference <scm|name>. If the preference is not
    defined the string <scm|"default"> is returned.
  </explain>

  <\explain>
    <scm|(preference-on? <scm-arg|name>)><explain-synopsis|test boolean user
    preference>
  <|explain>
    Returns <scm|#t> if the value of preference <scm|name> is <scm|"on">.
  </explain>

  <\explain>
    <scm|(toggle-preference <scm-arg|name>)><explain-synopsis|change value of
    boolean user preference>
  <|explain>
    Toggles the value of preference <scm|name> between <scm|"on"> and
    <scm|"off">.
  </explain>

  <tmdoc-copyright|2012|Miguel de Benito Delgado>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>