<TeXmacs|1.99.11>

<style|tmdoc>

<\body>
  <doc-data|<doc-title|Translating the user interface>>

  As an example, I will assume my own language: Dutch.

  The file with English to Dutch translations is

  <space|2em><slink|$TEXMACS_PATH/langs/natural/dic/english-dutch.scm>

  The file with all entries to be translated is

  <space|2em><slink|$TEXMACS_PATH/langs/natural/dic/english-new.scm>

  By executing

  <\session|scheme|default>
    <\input|Scheme] >
      (use-modules (utils misc translation-list))
    </input>

    <\input|Scheme] >
      (update-missing-for "dutch")
    </input>
  </session>

  you may create the following file with missing entries for Dutch:

  <space|2em><slink|$TEXMACS_PATH/langs/natural/miss/english-dutch-miss.scm>

  You may wish to perform as many translations as possible automatically,
  using on-line tools such as <name|Google Translate>, <name|DeepL Pro>,
  Collins free online translator, etc. For this, first construct an english
  source file for the files to be translated:

  <space|2em><slink|$TEXMACS_PATH/langs/natural/miss/list-english.scm>

  using

  <\session|scheme|default>
    <\input|Scheme] >
      (translate-begin "dutch")
    </input>
  </session>

  You next have to translate this file using your online tool and put the
  result in

  <space|2em><slink|$TEXMACS_PATH/langs/natural/miss/list-dutch.scm>

  Once you are done, you execute

  <\session|scheme|default>
    <\input|Scheme] >
      (translate-end "dutch")
    </input>
  </session>

  This command updates the file

  <space|2em><slink|$TEXMACS_PATH/langs/natural/miss/english-dutch-miss.scm>

  You next have to further correct this file. For testing the translations,
  simply put the correctly translated entries in

  <space|2em><slink|$TEXMACS_PATH/langs/natural/dic/english-dutch.scm>

  and restart <TeXmacs> (possibly using the option <verbatim|--setup> to
  clear the cache).
</body>

<initial|<\collection>
</collection>>