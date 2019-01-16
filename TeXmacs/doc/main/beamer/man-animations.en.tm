<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Animations>

  <TeXmacs> provides some rudimentary support for animations inside laptop
  presentations. This support is likely to be further improved in future
  <TeXmacs> distributions.

  The simplest animations are available from the menus
  <menu|Insert|Animation|Translate> and <menu|Insert|Animation|Progressive>.
  Using the first menu, it is possible to create moving content: you first
  specify a duration for the full animation and then enter the content which
  has to be moved. The different kinds of moving content are illustrated in
  figure<nbsp><reference|translate-fig>. Similarly, using the second menu, it
  is possible to create content which only progressively appears on the
  screen. The various kinds of progressive content are illustrated in
  figure<nbsp><reference|progressive-fig>. The duration of the animations can
  be <hlink|modified <em|a<nbsp>posteriori>|../editing/man-structured-geometry.en.tm>
  by putting your cursor inside them and using the shortcuts
  <shortcut|(geometry-left)> and<nbsp><shortcut|(geometry-right)>.

  <big-figure|<tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-translate|<large|Hello
  world>|2sec|<tuple|-1.0|0.0>|>>>|<row|<cell|<menu|Rightwards>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-translate|<large|Hello
  world>|2sec|<tuple|1.0|0.0>|>>>|<row|<cell|<menu|Leftwards>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-translate|<large|Hello
  world>|2sec|<tuple|0.0|-1.0>|>>>|<row|<cell|<menu|Upwards>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-translate|<large|Hello
  world>|2sec|<tuple|0.0|1.0>|>>>|<row|<cell|<menu|Downwards>>>>>>|<label|translate-fig>Moving
  content, as inserted from <menu|Insert|Animation|Translate>.>

  <big-figure|<tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-progressive|<large|Hello<anim-progressive||2sec|<tuple|0.0|0.0|0.0|1.0>|>>|2sec|<tuple|0.0|0.0|0.0|1.0>|>>>|<row|<cell|<menu|Rightwards>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-progressive|<large|Hello>|2sec|<tuple|1.0|0.0|1.0|1.0>|>>>|<row|<cell|<menu|Leftwards>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-progressive|<large|Hello
  world>|2sec|<tuple|0.5|0.5|0.5|0.5>|>>>|<row|<cell|<menu|From
  center>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-progressive|<large|Hello>|2sec|<tuple|0.0|0.0|1.0|0.0>|>>>|<row|<cell|<menu|Upwards>>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-progressive|<large|Hello>|2sec|<tuple|0.0|1.0|1.0|1.0>|>>>|<row|<cell|<menu|Downwards>>>>>>|<label|progressive-fig>Progressive
  content, as inserted from <menu|Insert|Animation|Progressive>.>

  Other basic animations are \Panimated gif pictures\Q, which can be inserted
  from <menu|Insert|Animation|Animation>, and sounds, which can be inserted
  from <menu|Insert|Animation|Sound>. Support for movies should be added
  later.

  It is also possible to combine animation, so as to form larger animations.
  For instance, using <menu|Insert|Animation|Compose> you can play several
  animations one after another. Often the individual elements of a composed
  animations are fixed animation of a given duration, which can be inserted
  using <menu|Insert|Animation|Fixed>. Of course, you may also use moving or
  progressive content or even composed animations as building blocks. An
  animation can be repeated indefinitely using
  <menu|Insert|Animation|Repeat>. This may for instance be used to create a
  blinking effect. Some examples of the various possibilities can be found in
  figure<nbsp><reference|compose-fig>. <rsub|<math|>>

  <big-figure|<tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-compose|<anim-constant|<large|Hello>|2sec>|<anim-constant|<large|World>|2sec>>>>|<row|<cell|Compose>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-repeat|<anim-compose|<anim-constant|<large|<space|0.6spc>Hello>|1sec>|<anim-constant|<large|World>|1sec>>>>>|<row|<cell|Blinking>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<large|<anim-repeat|<anim-compose|<anim-constant|<with|color|brown|T<space|-0.2spc><rsub|<with|math-level|0|font-shape|small-caps|e>>X<space|-0.2spc>><with|color|dark
  green|<rsub|<with|math-level|0|font-shape|small-caps|m<space|-0.2spc>a<space|-0.4spc>c<space|-0.2spc>s>>>|1sec>|<anim-constant|<with|color|dark
  green|T<space|-0.2spc><rsub|<with|color|brown|<with|math-level|0|font-shape|small-caps|e>>>X<space|-0.2spc>><with|color|brown|<rsub|<with|math-level|0|font-shape|small-caps|m<space|-0.2spc>a<space|-0.4spc>c<space|-0.2spc>s>>>|1sec>>>>>>|<row|<cell|<TeXmacs>
  logo>>>>><space|1em><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0.5em>|<cwith|1|1|1|1|cell-rsep|0.5em>|<cwith|1|1|1|1|cell-bsep|0.5em>|<cwith|1|1|1|1|cell-tsep|0.5em>|<cwith|1|1|1|1|cell-background|pastel
  yellow>|<cwith|2|2|1|1|cell-tsep|0.5em>|<table|<row|<cell|<anim-repeat|<large|<name|<anim-compose|<anim-progressive|<with|color|dark
  green|Mathe<rsub|<math|>>>|2sec|<tuple|0.0|0.0|0.0|1.0>|><phantom|magiX>|<anim-constant|<with|color|dark
  green|Mathe<rsub|<math|>>><with|color|dark
  magenta|magiX>|1sec>>>>>>>|<row|<cell|<name|Magix>
  animation>>>>>|<label|compose-fig>Different kinds of composed animations.>

  <tmdoc-copyright|2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>