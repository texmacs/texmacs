<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Example of a <name|Mathemagix> session>

  <assign|mmx-prompt|<macro|nr|<with|color|red|<arg|nr>]<specific|html|&nbsp;>
  >>><assign|mmx-prompt|<macro|nr|<with|mode|math|Mmx\<rangle\><specific|html|&nbsp;>>>><assign|mmx-prompt|<macro|nr|<with|prog-language|verbatim|Mmx]<specific|html|&nbsp;>
  >>>Here follows a sample session, which was started using
  <menu|Insert|Session|Mathemagix>. Detailed features are available from the
  <mathemagix> website at <hlink|<samp|http://www.mathemagix.org>|http://www.mathemagix.org/>.

  <\session|mathemagix|default>
    <\output>
      <tformat|<cwith|1|5|1|1|cell-halign|c>|<table|<row|<with|color|blue|<with|font-base-size|14|Welcome
      to Mathemagix-light 0.4>>>|<row|<with|color|blue|This software falls
      under the GNU General Public License>>|<row|<with|color|orange|It comes
      without any warranty whatsoever>>|<row|www.mathemagix.org>|<row|(c)
      2001-2010>>>
    </output>

    <\unfolded-io>
      <mmx-prompt|1>
    <|unfolded-io>
      n: Int == 10
    <|unfolded-io>
      <math|10>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|2>
    <|unfolded-io>
      p: Int == 3
    <|unfolded-io>
      <math|3>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|4>
    <|unfolded-io>
      n rem p
    <|unfolded-io>
      <math|1>
    </unfolded-io>

    <\input>
      <mmx-prompt|5>
    <|input>
      use "numerix"
    </input>

    <\unfolded-io>
      <mmx-prompt|8>
    <|unfolded-io>
      a: Integer == 200
    <|unfolded-io>
      <math|200>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|9>
    <|unfolded-io>
      a!
    <|unfolded-io>
      <math|788657867364790503552363213932185062295135977687173263294742533244359449963403342920304284011984623904177212138919638830257642790242637105061926624952829931113462857270763317237396988943922445621451664240254033291864131227428294853277524242407573903240321257405579568660226031904170324062351700858796178922222789623703897374720000000000000000000000000000000000000000000000000>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|13>
    <|unfolded-io>
      b: Floating == 3.14
    <|unfolded-io>
      <math|3.14000000000000000010>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|14>
    <|unfolded-io>
      exp b
    <|unfolded-io>
      <math|23.1038668587221827869>
    </unfolded-io>

    <\input>
      <mmx-prompt|15>
    <|input>
      use "algebramix"
    </input>

    <\unfolded-io>
      <mmx-prompt|16>
    <|unfolded-io>
      p: Polynomial Rational == polynomial (1,1)
    <|unfolded-io>
      <math|x+1>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|18>
    <|unfolded-io>
      p^10
    <|unfolded-io>
      <math|x<rsup|10>+10*x<rsup|9>+45*x<rsup|8>+120*x<rsup|7>+210*x<rsup|6>+252*x<rsup|5>+210*x<rsup|4>+120*x<rsup|3>+45*x<rsup|2>+10*x+1>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|19>
    <|unfolded-io>
      m: Matrix Rational == [1, 2; 3, 4]
    <|unfolded-io>
      <math|<around*|[|<tabular*|<table|<row|1|2>|<row|3|4>>>|]>>
    </unfolded-io>

    <\unfolded-io>
      <mmx-prompt|27>
    <|unfolded-io>
      m * m
    <|unfolded-io>
      <math|<around*|[|<tabular*|<table|<row|7|10>|<row|15|22>>>|]>>
    </unfolded-io>
  </session>

  <tmdoc-copyright|2012|Gregoire Lecerf>

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
  </collection>
</initial>