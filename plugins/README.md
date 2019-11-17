# Collection of GNU TeXmacs plugins
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Status of Plugins for GNU TeXmacs
| Plugin     | MacOS  | GNU/Linux | Windows | Language      | Last Checker | Date       |
|------------|--------|-----------|---------|---------------|--------------|------------|
| asymptote  | :100:  | :100:     | :100:   | Python(tmpy)  | @sadhen      | 2019-08-31 |
| axiom      |        | :x:       |         | Scheme/C      | @sadhen      | 2018-10-14 |
| cadabra    |        | 1.46      |         | Scheme        | @sadhen      | 2018-10-14 |
| coq        | :x:    |           |         | Scheme/C      | @sadhen      | 2018-10-20 |
| dratex     | :100:  | :100:     | :x:     | Python(tmpy)  | Pedro        | 2018-10-20 |
| eukleides  |        | :x:       | :x:     | Scheme/Shell  | Pedro        | 2018-10-14 |
| feynmf     |        | :x:       |         | Perl/Shell    | @sadhen      | 2018-10-14 |
| fricas     | 1.3.4  | 1.3.4     |         | Scheme        | @sadhen      | 2018-10-14 |
| giac       |        | 1.2.3.57  |         | Scheme        | @sadhen      | 2018-10-14 |
| gnuplot    | :100:  | :100:     | :100:   | Python(tmpy)  | @sadhen      | 2019-08-31 |
| graph      | :100:  | :100:     | :100:   | Python(tmpy)  | @sadhen      | 2019-08-31 |
| graphviz   | :100:  | :100:     | :100:   | Python(tmpy)  | @sadhen      | 2019-08-31 |
| macaulay2  | 1.12   |           |         | Scheme        | @sadhen      | 2018-10-14 |
| maxima     | 5.41   | 5.41      | 5.42.0  | Scheme/Lisp   | @sadhen      | 2018-10-14 |
| octave     | :x:    |           | 4.2.2   | Scheme/Shell  | Pedro        | 2018-10-20 |
| pari       | 2.11.0 | 2.9.5     | 2.11.0  | Scheme        | @sadhen      | 2018-10-14 |
| python     | :100:  | :100:     | :100:   | Python(tmpy)  | Pedro        | 2018-10-14 |
| sage       | 8.3    | 8.1       | :x:     | Python(tmpy)  | @sadhen      | 2018-10-14 |
| scheme     | :100:  | :100:     | :100:   | Scheme        | @sadhen      | 2018-10-14 |
| scilab     |        | :x:       | 6.0.1   | Scheme        | @sadhen      | 2018-10-14 |
| shell      | :100:  | :100:     |         | Scheme/C++    | @sadhen      | 2018-10-14 |
| xypic      | :100:  | :100:     | :100:   | Python(tmpy)  | @sadhen      | 2019-08-31 |
| yacas      | 1.6.1  | 1.3.6     |  1.3.6  | Scheme        | @sadhen      | 2018-10-20 |

**NOTE**: It is not a good idea to implement a plugin using Shell scripting. Ideally,
plugins should be implemented in Scheme(Guile).

## How to contribute
Documentation on how to interface TeXmacs with an extern application can be browsed
in the `Help->Interfacing` menu or [online](http://www.texmacs.org/tmweb/manual/webman-write-itf.en.html).

