# How to use ?

## Windows using msys2/mingw

1. Install [msys2](https://www.msys2.org/)
2. Open any terminal, such as `clangarm64` (for ARM PC) or `mingw64` (for x86_64 PC)
3. Run `./scripts/build`

The produced environment is located under `/windows-qt6`.  
TeXmacs is located under `/windows-qt6/texmacs/src`.

You can re-use this environment by:
```bash
cd /windows-qt6
source set-devel-path
mingw32-make
```
