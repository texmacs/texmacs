<TeXmacs|1.0.0.6>

<style|<tuple|generic|giac>>

<\body>
  <\session|giac|default>
    <\output>
      ---------------------------------------------------------------

      Welcome to the giac computer algebra system for TeXmacs

      Released under the GPL license

      (c) 2001 by J. van der Hoeven (TeXmacs), B. Parisse & al (giac)

      ---------------------------------------------------------------

      \;
    </output>

    <\input|quest(0) >
      diff(x^x^x,x);
    </input>

    <\input|quest(1) >
      integrate((x^3+x-1)/(x^2+3*x-7),x);
    </input>

    <\input|quest(2) >
      M:=[[1,2],[3,4]];
    </input>

    <\input|quest(3) >
      series(sin(sin(x)),x,0)
    </input>

    <\input|quest(4) >
      plotfunc(sin(x),x)
    </input>

    <\input|quest(5) >
      plotfunc(sin(x+y*y/3),[x,y]);
    </input>

    <\input|quest(6) >
      \;
    </input>
  </session>

  \;
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|5mm>
    <associate|page medium|automatic>
    <associate|shrinking factor|4>
    <associate|page right margin|5mm>
    <associate|page top margin|5mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|5mm>
    <associate|reduction page left margin|25mm>
    <associate|page height|757760unit>
    <associate|page bottom margin|5mm>
    <associate|reduction page top margin|15mm>
    <associate|page width|1015808unit>
    <associate|language|english>
  </collection>
</initial>
