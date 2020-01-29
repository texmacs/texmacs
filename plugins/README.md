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
| macaulay2  | 1.15   |           |         | Scheme        | @sadhen      | 2020-01-29 |
| maxima     | 5.43.0 | 5.41      | 5.42.0  | Scheme/Lisp   | @sadhen      | 2020-01-29 |
| octave     | 5.1.0  |           | 4.2.2   | Scheme/Shell  | @sadhen      | 2020-01-29 |
| pari       | 2.11.2 | 2.9.5     | 2.11.0  | Scheme        | @sadhen      | 2020-01-29 |
| python     | :100:  | :100:     | :100:   | Python(tmpy)  | Pedro        | 2018-10-14 |
| sage       | 8.3    | 8.1       | :x:     | Python(tmpy)  | @sadhen      | 2018-10-14 |
| scheme     | :100:  | :100:     | :100:   | Scheme        | @sadhen      | 2018-10-14 |
| scilab     |        | :x:       | 6.0.1   | Scheme        | @sadhen      | 2018-10-14 |
| shell      | :100:  | :100:     |         | Scheme/C++    | @sadhen      | 2018-10-14 |
| xypic      | :100:  | :100:     | :100:   | Python(tmpy)  | @sadhen      | 2019-08-31 |
| yacas      | 1.8.0  | 1.3.6     |  1.3.6  | Scheme        | @sadhen      | 2020-01-29 |

**NOTE**: It is not a good idea to implement a plugin using Shell scripting. Ideally,
plugins should be implemented in Scheme(Guile).

## Trouble Shootings
### Octave
For GNU TeXmacs 1.99.12 on macOS, we have to install the missing .octaverc via

``` bash
wget https://github.com/texmacs/plugins/raw/master/octave/octave/.octaverc -O /Applications/TeXmacs-1.99.12.app/Contents/Resources/share/TeXmacs/plugins/octave/octave/.octaverc
```

or (It is a pity that Github is not always available for some areas)

``` bash
wget https://gitee.com/texmacs/plugins/raw/master/octave/octave/.octaverc -O /Applications/TeXmacs-1.99.12.app/Contents/Resources/share/TeXmacs/plugins/octave/octave/.octaverc
```

### PariGP
For GNU TeXmacs 1.99.12 on macOS, please add `/Applications/PariGP.app/Contents/Resources/bin/` to your `PATH`. And do **NOT** install PariGP using Homebrew.


## How to contribute
Documentation on how to interface TeXmacs with an extern application can be browsed
in the `Help->Interfacing` menu or [online](http://www.texmacs.org/tmweb/manual/webman-write-itf.en.html).

