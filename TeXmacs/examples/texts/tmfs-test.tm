<TeXmacs|1.99.11>

<style|generic>

<\body>
  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tmfs-load-handler (test what)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document "\\\\"))))
    <|unfolded-io>
      #\<less\>procedure #f (what)\<gtr\>
    </unfolded-io>

    <\folded-io|Scheme] >
      (load-document "tmfs://test/1")
    <|folded-io>
      \;
    </folded-io>

    <\unfolded-io|Scheme] >
      "\\\\"
    <|unfolded-io>
      "\\\\"
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  The <scm|load-document> action should open a new <TeXmacs> buffer with two
  backward slashes.
</body>

<initial|<\collection>
</collection>>
