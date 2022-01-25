# GNU TeXmacs (experimental branch S7/Qt/Widkit/Emscripten)

January 2022.

This is *experimental code*. Use at your own risk.

This branch uses S7 Scheme and the TeXmacs own widget kit (Widkit) on top of a minimal Qt5/6 support. The idea is that this combination allows an easy porting to Web Assembly via the Qt support for Wasm. 

Ideally one could get rid also of Qt and use SDL or GLFW but we would need anyway some library for graphics rendering. For the moment we stick to Qt which provide an easy access to the Wasm platform. 

The build system is not yet ready. Maybe the following incantation will work for you after some tinkering, provided you have the `emscripten`SDK and Qt6/Wasm properly installed. (it can work also with Qt5 I believe).

First we copy the `CMakeList.txt` file for Qt6, you will have to change some of the hardcoded paths to fit your system.
```
cp packages/wasm/CMakeList.txt.qt6 CMakeLists.txt
```
Then we prepare the build (`emcmake/emmake/emrun` are utilities from the emscripten SDK)
```
emcmake cmake -S . -B build-wasm 
```
and now we perform the actual build
```
cd build-wasm
emmake make
```
Once finished one can run it with
```
emrun --browser firefox TeXmacs.html
```

Below follows the standard README.md for TeXmacs/S7 and the original TeXmacs

-----------------------


# GNU TeXmacs (experimental branch for S7 Scheme)

January 2021.

This is *experimental code*. Use at your own risk. In particular do not assume that ./configure do the right job, it might not.
This version of TeXmacs uses [S7 scheme](https://cm-gitlab.stanford.edu/bil/s7.git) instead of Guile. Currenlty seems to run fine, however it still has to be considered not ready for production. The main changes are in the `src/Scheme` and in the `TeXmacs/progs` directories. In the future I will have scheme code which works with both Guile and S7.

At this point performance is good. TeXmacs/S7 compiles the full user manual in 20 sec while TeXmacs/Guile-1.8 in 15 sec. However there should be some margin for improvement. S7 is much faster than Guile 1.8 in standard benchmarks. 

Update 4.1.2021: I've found a trick to improve lookup of symbols in S7 and now TeXmacs/S7 compiles the manual in the same time as TeXmacs/Guile (15 sec). Moreover startup time is reduced by half. So we have similar performances in these tasks.


For the standard r7rs benchmarks [here](https://github.com/ecraven/r7rs-benchmarks) I got the following results on my MacBook Air (2019). 


| test                           |      s7 | chibi-0.9.1 | chez-9.5.1 | guile-1.8.8 | guile-3.0.4 |
|--------------------------------|--------:|------------:|-----------:|------------:|------------:|
| browse:2000                    |   24.27 |     TIMELIM |       0.84 |       76.63 |       12.06 |
| deriv:10000000                 |   25.19 |       97.11 |       1.15 |       67.27 |       18.58 |
| destruc:600:50:4000            |   52.08 |       94.90 |       2.21 |     TIMELIM |        7.14 |
| diviter:1000:1000000           |    9.69 |       55.88 |       1.81 |       82.75 |       15.45 |
| divrec:1000:1000000            |   11.80 |       50.91 |       2.19 |       82.04 |       17.41 |
| puzzle:1000                    |   27.72 |      270.98 |       1.66 |      221.49 |       18.09 |
| triangl:22:1:50                |   33.93 |      110.86 |       1.91 |      107.15 |        8.52 |
| tak:40:20:11:1                 |   12.93 |       55.19 |       1.66 |      134.21 |        4.76 |
| takl:40:20:12:1                |   20.97 |     TIMELIM |       3.71 |     TIMELIM |        9.46 |
| ntakl:40:20:12:1               |   17.07 |       95.98 |       3.65 |     TIMELIM |        9.52 |
| cpstak:40:20:11:1              |  103.36 |      222.13 |       4.21 |      258.62 |       59.44 |
| ctak:32:16:8:1                 |   44.14 |     TIMELIM |       0.96 |     TIMELIM |     TIMELIM |
| fib:40:5                       |   10.22 |       96.25 |       3.63 |      236.65 |       12.09 |
| fibc:30:10                     |   25.80 |     TIMELIM |       0.71 |     TIMELIM |     TIMELIM |
| fibfp:35.0:10                  |    1.89 |       42.79 |       3.18 |       56.26 |       22.00 |
| sum:10000:200000               |    6.64 |       99.79 |       3.59 |     TIMELIM |        6.87 |
| sumfp:1000000.0:500            |    2.50 |       85.36 |       4.25 |      111.78 |       42.06 |
| fft:65536:100                  |   32.20 |       69.76 |       3.32 |     TIMELIM |        7.69 |
| mbrot:75:1000                  |   24.40 |      209.51 |       5.39 |     TIMELIM |       50.09 |
| mbrotZ:75:1000                 |   18.56 |     TIMELIM |       9.26 |     TIMELIM |       67.01 |
| nucleic:50                     |   19.95 |       79.05 |       2.59 |       69.32 |       15.35 |
| pi                             |      NO |     TIMELIM |       0.60 |     TIMELIM |        0.56 |
| pnpoly:1000000                 |   17.98 |      253.84 |       4.73 |     TIMELIM |       24.89 |
| ray:50                         |   20.46 |      119.63 |       2.83 |     TIMELIM |       18.51 |
| simplex:1000000                |   46.34 |      182.76 |       2.34 |     TIMELIM |       13.90 |
| ack:3:12:2                     |   10.57 |       74.51 |       3.12 |     TIMELIM |        8.41 |
| array1:1000000:500             |   11.48 |       64.18 |       8.16 |      138.45 |        9.24 |
| string:500000:100              |    1.71 |        6.34 |       6.54 |        1.81 |        1.87 |
| sum1:25                        |    0.47 |      121.78 |       2.02 |        1.71 |        4.43 |
| cat:50                         |    1.19 |       70.95 |       2.69 |     TIMELIM |       28.40 |
| tail:50                        |    1.19 |       11.21 |       3.57 |     TIMELIM |        9.82 |
| wc:inputs/bib:50               |    8.27 |     TIMELIM |       1.76 |       73.34 |       16.96 |
| read1:2500                     |    0.41 |      281.23 |       1.28 |        2.69 |        5.80 |
| compiler:2000                  |   41.16 |      115.83 |       2.99 |     TIMELIM |        5.15 |
| conform:500                    |   51.03 |      199.42 |       2.10 |     TIMELIM |       10.51 |
| dynamic:500                    |   22.74 |      288.25 |       4.01 |       71.60 |        7.37 |
| earley                         | TIMELIM |     TIMELIM |       5.03 |     TIMELIM |        9.49 |
| graphs:7:3                     |  127.61 |     TIMELIM |       2.27 |     TIMELIM |       23.03 |
| lattice:44:10                  |  139.28 |     TIMELIM |       3.91 |     TIMELIM |       15.94 |
| matrix:5:5:2500                |   72.07 |      214.16 |       1.63 |     TIMELIM |        9.88 |
| maze:20:7:10000                |   23.26 |       84.15 |       1.66 |     TIMELIM |        4.70 |
| mazefun:11:11:10000            |   19.51 |      122.02 |       2.86 |      128.66 |        9.66 |
| nqueens:13:10                  |   55.11 |      165.25 |       4.99 |     TIMELIM |       19.37 |
| paraffins:23:10                |   31.42 |     TIMELIM |       5.97 |     TIMELIM |        4.25 |
| parsing:2500                   |   39.44 |     TIMELIM |       3.35 |     TIMELIM |       10.69 |
| peval:2000                     |   29.68 |      177.10 |       2.05 |      107.05 |       15.64 |
| primes:1000:10000              |    7.73 |       21.08 |       2.64 |       43.73 |        7.52 |
| quicksort:10000:2500           |   94.00 |     TIMELIM |       3.90 |     TIMELIM |       13.25 |
| scheme:100000                  |   71.46 |      154.36 |       2.55 |     TIMELIM |       15.14 |
| slatex:500                     |   32.07 |      173.60 |       3.73 |       43.82 |       45.05 |
| chudnovsky                     |      NO |     TIMELIM |       0.32 |     TIMELIM |        0.31 |
| nboyer:5:1                     |   39.27 |       41.54 |       3.95 |      142.86 |        5.10 |
| sboyer:5:1                     |   31.54 |       39.14 |       1.30 |      155.49 |        4.76 |
| gcbench:20:1                   |   20.54 |     TIMELIM |       1.87 |     TIMELIM |        3.51 |
| mperm:20:10:2:1                |  173.33 |      659.14 |      13.33 |     TIMELIM |       10.65 |
| equal:100:100:8:1000:2000:5000 |    0.78 |       54.11 |       0.99 |     TIMELIM |     TIMELIM |
| bv2string:1000:1000:100        |   10.78 |       11.17 |       2.47 |     TIMELIM |        4.49 |




Below follows the standard README.md for TeXmacs

-----------------------
# GNU TeXmacs
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[GNU TeXmacs](https://texmacs.org) is a free wysiwyw (what you see is what you want) editing platform with special features for scientists. The software aims to provide a unified and user friendly framework for editing structured documents with different types of content (text, graphics, mathematics, interactive content, etc.). The rendering engine uses high-quality typesetting algorithms so as to produce professionally looking documents, which can either be printed out or presented from a laptop.

The software includes a text editor with support for mathematical formulas, a small technical picture editor and a tool for making presentations from a laptop. Moreover, TeXmacs can be used as an interface for many external systems for computer algebra, numerical analysis, statistics, etc. New presentation styles can be written by the user and new features can be added to the editor using the Scheme extension language. A native spreadsheet and tools for collaborative authoring are planned for later.

TeXmacs runs on all major Unix platforms and Windows. Documents can be saved in TeXmacs, Xml or Scheme format and printed as Postscript or Pdf files. Converters exist for TeX/LaTeX and Html/Mathml. 

## Documentation
GNU TeXmacs is self-documented. You may browse the manual in the `Help` menu or browse the online [one](https://www.texmacs.org/tmweb/manual/web-manual.en.html).

For developer, see [this](./COMPILE) to compile the project.

## Contributing
Please report any [new bugs](https://www.texmacs.org/tmweb/contact/bugs.en.html) and [suggestions](https://www.texmacs.org/tmweb/contact/wishes.en.html) to us. It is also possible to [subscribe](https://www.texmacs.org/tmweb/help/tmusers.en.html) to the <texmacs-users@texmacs.org> mailing list in order to get or give help from or to other TeXmacs users.

You may contribute patches for TeXmacs using the [patch manager](http://savannah.gnu.org/patch/?group=texmacs) on Savannah or using the [pull request](https://github.com/texmacs/texmacs/pulls) on Github. Since we are using SVN on Savannah, PRs won't be directly accepted on Github. We will `git apply` the patch into SVN repo if the PR is accepted. And we will close the PR and change the title to `[SVN] xxx` after applying the PR.
