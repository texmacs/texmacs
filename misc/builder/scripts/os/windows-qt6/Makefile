all: check local/lib/libltdl.a local/lib/libiconv.a local/lib/libpng16.a local/lib/libfreetype.a local/lib/libgmp.a local/bin/qmake ghostscript texmacs

.PHONY: check texmacs

check:
	@if [ -z "$$WORKING_DIR" ]; then echo "Please source set-devel-paths"; exit; fi;

libtool/Makefile: check
	cd libtool; ./configure --prefix=$$WORKING_DIR/local --disable-silent-rules --enable-static --disable-shared --with-gnu-ld=yes

libtool/libltdl/.libs/libltdl.a: check libtool/Makefile
	cd libtool; $(MAKE)

local/lib/libltdl.a: check libtool/libltdl/.libs/libltdl.a
	cd libtool; $(MAKE) install
	if [ -f "local/lib/libltdl.a" ]; then echo "libltdl.a has been installed"; else echo "error on libltdl"; exit; fi;

libiconv/Makefile: check
	cd libiconv; ./configure --enable-static --disable-shared --prefix=$$WORKING_DIR/local

libiconv/lib/.libs/libiconv.a: check libiconv/Makefile
	cd libiconv; $(MAKE)

local/lib/libiconv.a: check libiconv/lib/.libs/libiconv.a
	cd libiconv; $(MAKE) install

libpng/Makefile: check local/lib/libltdl.a
	cd libpng; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes

libpng/.libs/libpng16.a: check libpng/Makefile
	cd libpng; $(MAKE)

local/lib/libpng16.a: check libpng/.libs/libpng16.a
	cd libpng; $(MAKE) install
	if [ -f "local/lib/libpng16.a" ]; then echo "libpng16.a has been installed"; else echo "error on libpng"; exit; fi;

freetype/build/Makefile: check local/lib/libltdl.a local/lib/libpng16.a
	cd freetype; mkdir build; cd build; cmake -DCMAKE_POLICY_VERSION_MINIMUM=3.5 -DCMAKE_INSTALL_PREFIX=$$WORKING_DIR/local -DFT_DISABLE_BZIP2=ON -DFT_DISABLE_BROTLI=ON -DFT_DISABLE_HARFBUZZ=ON -DCMAKE_OSX_DEPLOYMENT_TARGET=$$OSX_TARGET ..

freetype/build/libfreetype.a: check freetype/build/Makefile
	cd freetype/build; cmake --build .

local/lib/libfreetype.a: check freetype/build/libfreetype.a
	cd freetype/build; cmake --build . --target install
	if [ -f "local/lib/libfreetype.a" ]; then echo "libfreetype.a has been installed"; else echo "error on libfreetype"; exit; fi;

gmp/build/Makefile: check local/lib/libltdl.a
	cd gmp; mkdir -p build; cd build; ../configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes --enable-assembly=no

gmp/build/.libs/libgmp.a: check gmp/build/Makefile
	cd gmp/build; $(MAKE)

local/lib/libgmp.a: check gmp/build/.libs/libgmp.a
	cd gmp/build; $(MAKE) install

local/bin/qmake: check
	cp patched/QtPkgConfigHelpers.cmake qt/qtbase/cmake/QtPkgConfigHelpers.cmake 
	-mkdir qt-build
	cd qt-build; ../qt/configure -opensource -confirm-license -prefix $$WORKING_DIR/local -release -pkg-config -no-warnings-are-errors
	cd qt-build; cmake --build . --parallel
	cd qt-build; cmake --build . --target install --parallel

ghostscript/Makefile: check local/bin/qmake local/lib/libfreetype.a
	chmod +x ghostscript/configure
	chmod +x ghostscript/base/instcopy
	cd ghostscript; LIBS=$$LDFLAGS CFLAGS="$$CFLAGS -Wno-implicit-int -Wno-implicit-function-declaration -D__CRT__NO_INLINE" CPPFLAGS="$$CPPFLAGS -DOPJ_EXPORTS" ./configure --prefix=$$WORKING_DIR/local --without-x --with-drivers=FILES --disable-cups --disable-gtk --without-tesseract --with-libiconv=gnu --disable-threading --disable-byteswap-h --disable-fontconfig --disable-dbus --with-libtiff

ghostscript: check ghostscript/Makefile
	cd ghostscript; $(MAKE) -j1; $(MAKE) install
	cp local/bin/gs.exe local/windows/bin

texmacs: check
	cd texmacs/src; autoreconf; PKG_CONFIG_PATH=$$WORKING_DIR/local/lib/pkgconfig ./configure --with-texmacs-builder=$$WORKING_DIR/local --prefix=$$WORKING_DIR/local --with-guile=embedded18 --with-qt-find-method=pkgconfig; mingw32-make; mingw32-make PACKAGE
