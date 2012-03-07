<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Inserting new objects>

  After insertion of a new picture or clicking inside an existing picture,
  the second mode dependent toolbar shows a<nbsp>list of icons which are
  useful in graphics mode. In particular, the second group of icons
  <icon|tm_point_mode.xpm>, <icon|tm_line_mode.xpm>,
  <icon|tm_cline_mode.xpm>, <icon|tm_spline_mode.xpm>,
  <icon|tm_cspline_mode.xpm>, <icon|tm_arc_mode.xpm>,
  <icon|tm_carc_mode.xpm>, <icon|tm_textat_mode.xpm>, <icon|tm_math.xpm> on
  this toolbar allows you to select the kind of objects that you want to
  insert. <TeXmacs> currently implements the following primitive graphical
  objects:

  <\description>
    <item*|Points>When selecting point mode using <icon|tm_point_mode.xpm> or
    <menu|Insert|Point>, you may insert simple points with the left mouse
    button.

    <item*|Lines and polygons>When selecting line mode using
    <icon|tm_line_mode.xpm> or <menu|Insert|Line>, you may insert a new
    broken line with the left mouse button: at every new click a new point is
    inserted and the last point is inserted using a double click. Polygon
    mode (<icon|tm_cline_mode.xpm> or <menu|Insert|Polygon>) is a variant of
    line mode, with this difference that an additional segment is inserted
    between the first and the last points.

    <item*|Splines and closed splines>Spline mode is selected using
    <icon|tm_spline_mode.xpm> or <menu|Insert|Spline>. This mode is similar
    to line mode, except that we now draw a smooth curve through the
    specified points. Again, this mode admits a closed variant
    (<icon|tm_cspline_mode.xpm> or <menu|Insert|Closed spline>).

    <item*|Arcs and circles>Arc mode is selected using <icon|tm_arc_mode.xpm>
    or <menu|Insert|Arc>. In this mode, you may insert arcs going through
    three points specified through left mouse clicks. Similarly, you may use
    circle mode (<icon|tm_carc_mode.xpm> or <menu|Insert|Circle>) for drawing
    circles.

    <item*|Text and mathematics>When selecting text mode
    (<icon|tm_textat_mode.xpm> or <menu|Insert|Text>) or mathematics mode
    (<icon|tm_math.xpm> or <menu|Insert|Mathematics>), you may enter text (or
    mathematics) at an arbitrary position in the picture, again using the
    left mouse button.
  </description>

  Typical examples of these basic objects are shown in the figures below:

  <\center>
    <tabular*|<tformat|<table|<row|<cell|<small-figure|<with|gr-mode|point|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<point|0.499041|-0.00699497>|<point|-0.919136|-0.23983>|<point|0.329706|-1.19234>>>|Points.>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|line>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<line|<point|-0.926313|0.839678>|<point|0.809366318296071|0.564509194337875>|<point|-0.862812541341447|-0.13399589892843>|<point|1.04220134938484|-0.260996824976849>|<point|-0.121973806059003|-1.17117012832385>>>>|Lines.>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|cline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<cline|<point|-0.742988|0.839678>|<point|0.844523085064162|0.691510120386294>|<point|0.886856727080302|-0.769000529170525>|<point|-0.425486175420029|-1.2770042333642>|<point|-1.25099219473475|-0.13399589892843>>>>|Polygons.>>>>>>

    \;

    <tabular*|<tformat|<table|<row|<cell|<small-figure|<with|gr-mode|<tuple|edit|spline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<spline|<point|-1.1308|1.0844>|<point|0.880043656568329|0.661066278608281>|<point|-0.855635666093399|-0.0374388146580235>|<point|0.901210477576399|-0.48194205582749>|<point|-0.516966529964281|-1.1804471490938>>>>|Splines.>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|cspline>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<cspline|<point|-0.651144|0.788067>|<point|0.89403360232835|0.322397142479164>|<point|0.640031750231512|-0.905278475988887>|<point|-0.566477047228469|-1.20161397010187>|<point|-1.15914803545443|-0.206773382722582>>>>|Closed
    splines.>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|arc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<arc|<point|-0.827656|-1.15928>|<point|0.865689906072232|-0.5454425188517>|<point|-0.806488953565286|0.87273448868898>>>>|Arcs.>>>>>>

    \;

    <tabular*|<tformat|<table|<row|<cell|<small-figure|<with|gr-mode|<tuple|edit|carc>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<carc|<point|-0.262964677867443|0.990458393967456>|<point|1.02821140362482|-0.0467158354279667>|<point|-0.0724632887948141|-1.1473905278476>>>>|Circles.>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|text-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<text-at|Hello|<point|-0.164307|0.545955>>>>|Text.>>|<cell|<small-figure|<with|gr-mode|<tuple|edit|math-at>|gr-frame|<tuple|scale|1cm|<tuple|0.5gw|0.5gh>>|gr-geometry|<tuple|geometry|3cm|3cm|center>|<graphics||<math-at|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=-1|<point|-1.08166|-0.533553>>>>|Mathematics.>>>>>>
  </center>

  <htab|5mm>

  <tmdoc-copyright|2012|Joris van der Hoeven>

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