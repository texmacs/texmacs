<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Control flow primitives>

  <\explain>
    <explain-macro|if|condition|if-body>

    <explain-macro|if|condition|if-body|else-body><explain-synopsis|conditional
    markup>
  <|explain>
    This primitive can be used to typeset <src-arg|if-body> only if the
    <src-arg|condition> is satisfied. If the optional <src-arg|else-body> is
    specified, then it is typeset if and only if the <src-arg|condition>
    fails.

    <\remark>
      It should be noticed that the use of conditional markup can be a bit
      tricky due to the fact that the accessibility of arguments cannot
      necessarily be checked beforehand. For instance, in the macro
      definition

      <\tm-fragment>
        <inactive*|<macro|x|<if|<visibility-flag>|<arg|x>>>>
      </tm-fragment>

      the macro argument <src-arg|x> is accessible if and only if
      <inactive*|<visibility-flag>> evaluates to true. This condition cannot
      necessarily be checked <em|a priori>. For certain editing operations,
      like searches or spell checking, the incorrect determination of the
      accessibility may lead to the positioning of the cursor at unaccessible
      places, or to the ignorance of certain markup. In the future, we plan
      to improve this aspect of the editor, but it is better to avoid
      conditional markup whenever another solution can be found.
    </remark>

    <\remark>
      The conditional constructs are only fully implemented for inline
      markup. In the case when you need conditional markup for block
      structures you currently have to write macros for the if-case and the
      else-case and use the <markup|compound> tag. For instance:

      <\tm-fragment>
        <inactive*|<assign|cold|<macro|x|<with|color|blue|<arg|x>>>>>

        <inactive*|<assign|hot|<macro|x|<with|color|red|<arg|x>>>>>

        <inactive*|<assign|adaptive|<macro|x|<compound|<if|<summer>|<value|hot>|<value|cold>>|<arg|x>>>>>
      </tm-fragment>
    </remark>
  </explain>

  <\explain>
    <explain-macro|case|cond-1|body-1|<math|\<cdots\>>|cond-n|body-n>

    <explain-macro|case|cond-1|body-1|<math|\<cdots\>>|cond-n|body-n|else-body><explain-synopsis|case
    distinction>
  <|explain>
    These commands are respectively equivalent to

    <\tm-fragment>
      <inactive*|<if|<arg|cond-1>|<arg|body-1>|<active*|<math|\<cdots\>>><if|<arg|cond-n>|<arg|body-n>>>>

      <inactive*|<if|<arg|cond-1>|<arg|body-1>|<active*|<math|\<cdots\>>><if|<arg|cond-n>|<arg|body-n>|<arg|else-body>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|while|condition|body><explain-synopsis|repeated
    evaluation>
  <|explain>
    This construct maybe used in order to repeatedly execute a given
    <src-arg|body> while a given <src-arg|condition> is satisfied. For
    instance, when declaring

    <\tm-fragment>
      <inactive*|<assign|count|<macro|from|to|<with|i|<arg|from>|<style-with|src-compact|none|<while|<less|<value|i>|<arg|to>>|<value|i>,
      <assign|i|<plus|<value|i>|1>>><arg|to>>>>>>
    </tm-fragment>

    the code <inactive*|<count|1|50>> produces

    <\tm-fragment>
      <with|count|<macro|from|to|<with|i|<arg|from>|<while|<less|<value|i>|<arg|to>>|<value|i>,
      <assign|i|<plus|<value|i>|1>>><arg|to>>>|<count|1|50>>
    </tm-fragment>
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>