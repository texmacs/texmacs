<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Crear sus propios menús dinámicos>

  En particular, el archivo de inicialización por defecto ejecuta

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu/main_menu.scm
  </verbatim>

  a fin de configurar el menú principal de <TeXmacs>. Le sugerimos echar un
  vistazo a este archivo a fin de ver cómo son creados los menús.

  De hecho, cualquier menú o parte de un menú es representada por un
  programa. El programa consiste de una lista de programas de una de las
  siguientes formas:

  <\verbatim>
    \ \ \ \ (=\<gtr\> "pulldown menu name" menu-definition)<format|next line>
    \ \ \ (-\<gtr\> "pullright menu name" menu-definition)<format|next line>
    \ \ \ ("entrada" accion)<format|next line> \ \ \ ("entrada" "shorthand"
    accion)<format|next line> \ \ \ ---<format|next line> \ \ \ (if condition
    menu-definition)<format|next line> \ \ \ (link variable)
  </verbatim>

  Los constructores <verbatim|=\<gtr\>> y <verbatim|-\<gtr\>> son usados para
  crear un menú desplegable hacia abajo o un menú desplegable hacia la
  derecha y la condición <verbatim|menu-definition> debe contener un programa
  que crea un submenú. El constructor <verbatim|("entrada" accion)> crea una
  entrada ordinaria, donde la <verbatim|accion> será compilado y ejecutado
  cuando haga click en la <verbatim|entrada>. El código opcional
  <verbatim|"shorthand"> se establece para un macro de teclado con la misma
  acción. Los items de un menú pueden separarse usando <verbatim|--->. El
  constructor <verbatim|if> es usado para insertar items de menú sólo si una
  cierta condición es satisfecha (por ejemplo, si estamos en el modo
  matemático).

  Finalmente, si hemos declarado una variable <TeXmacs> ser un menú mediante

  <\verbatim>
    \ \ \ \ (define variable menu-definition)
  </verbatim>

  entonces podemos usar este menú indirectamente usando el constructor
  <verbatim|link>. esta forma indirecta de declarar submenús tiene dos
  ventajas

  <\itemize>
    <item>Un menú ``indirecto'' puede ser vinculado a tantos menús como nos
    guste.

    <item>Nuevos items pueden ser adicionados a los submenús ``indirectos''
    <with|font shape|italic|a posteriori> usando\ 

    <\verbatim>
      \ \ \ \ (set! variable (menu-merge variable menu-declaration))
    </verbatim>

    De hecho, los términos existentes pueden ser también sobrescritos en esta
    forma.
  </itemize>

  Algunos menús indirectos estándar en <TeXmacs> son <verbatim|texmacs-menu>,
  <verbatim|file-menu>, <verbatim|edit-menu>, <verbatim|insert-menu>,
  <verbatim|text-menu>, <verbatim|paragraph-menu>, <verbatim|document-menu>,
  <verbatim|options-menu> y <verbatim|help-menu>. El comando

  <\verbatim>
    \ \ \ \ (menu-main menu-declaration)
  </verbatim>

  es usado a fin de configurar de hecho el menú principal. Por ejemplo, en la
  inicialización, ejecutamos

  <\verbatim>
    \ \ \ \ (menu-main '(link texmacs-menu))
  </verbatim>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|spanish>
  </collection>
</initial>
