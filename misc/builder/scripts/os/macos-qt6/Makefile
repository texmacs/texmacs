all: local/lib/libltdl.a local/lib/libiconv.a local/lib/libintl.a local/lib/libpng16.a local/lib/libfreetype.a local/lib/libgmp.a local/bin/qmake ghostscript texmacs

.PHONY: texmacs

libtool/Makefile:
	cd libtool; ./configure --prefix=$$WORKING_DIR/local --disable-silent-rules --enable-static --disable-shared --with-gnu-ld=yes

libtool/libltdl/.libs/libltdl.a: libtool/Makefile
	cd libtool; $(MAKE)

local/lib/libltdl.a: libtool/libltdl/.libs/libltdl.a
	cd libtool; $(MAKE) install
	if [ -f "local/lib/libltdl.a" ]; then echo "libltdl.a has been installed"; else echo "error on libltdl"; exit; fi;

libiconv/Makefile:
	cd libiconv; ./configure --enable-static --disable-shared --prefix=$$WORKING_DIR/local

libiconv/lib/.libs/libiconv.a: libiconv/Makefile
	cd libiconv; make

local/lib/libiconv.a: libiconv/lib/.libs/libiconv.a
	cd libiconv; make install

gettext/Makefile:
	cd gettext; ./configure --enable-static --disable-shared --prefix=$$WORKING_DIR/local

gettext/gettext-runtime/intl/.libs/libintl.a: gettext/Makefile
	cd gettext; make

local/lib/libintl.a: gettext/gettext-runtime/intl/.libs/libintl.a
	cd gettext; make install

libpng/Makefile:
	cd libpng; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes

libpng/.libs/libpng16.a: libpng/Makefile
	cd libpng; $(MAKE)

local/lib/libpng16.a: libpng/.libs/libpng16.a
	cd libpng; $(MAKE) install
	if [ -f "local/lib/libpng16.a" ]; then echo "libpng16.a has been installed"; else echo "error on libpng"; exit; fi;

freetype/build/Makefile: local/lib/libpng16.a
	cd freetype; mkdir build; cd build; cmake -G"Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$$WORKING_DIR/local -DFT_DISABLE_BZIP2=ON -DFT_DISABLE_BROTLI=ON -DFT_DISABLE_HARFBUZZ=ON -DCMAKE_OSX_DEPLOYMENT_TARGET=$$OSX_TARGET ..

freetype/build/libfreetype.a: freetype/build/Makefile
	cd freetype/build; cmake --build .

local/lib/libfreetype.a: freetype/build/libfreetype.a
	cd freetype/build; cmake --build . --target install
	if [ -f "local/lib/libfreetype.a" ]; then echo "libfreetype.a has been installed"; else echo "error on libfreetype"; exit; fi;

gmp/build/Makefile:
	cd gmp; mkdir -p build; cd build; ../configure --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes --enable-assembly=no

gmp/build/.libs/libgmp.a: gmp/build/Makefile
	cd gmp/build; $(MAKE)

local/lib/libgmp.a: gmp/build/.libs/libgmp.a
	cd gmp/build; $(MAKE) install

local/bin/qmake:
	-rm -rf qt-build
	-mkdir qt-build
	cd qt-build; ../qt/configure -opensource -confirm-license -prefix $$WORKING_DIR/local -release -nomake tests -nomake examples -no-warnings-are-errors -pkg-config -skip qtwebengine -- -G Ninja -DCMAKE_OSX_DEPLOYMENT_TARGET=$$OSX_TARGET && cmake --build . --parallel && cmake --build . --target install --parallel

ghostscript/Makefile: local/bin/qmake local/lib/libfreetype.a
	chmod +x ghostscript/configure
	chmod +x ghostscript/base/instcopy
	cd ghostscript; LIBS=$$LDFLAGS CFLAGS="$$CFLAGS -Wno-implicit-int -Wno-implicit-function-declaration -D__CRT__NO_INLINE" CPPFLAGS="$$CPPFLAGS -DOPJ_EXPORTS" ./configure --prefix=$$WORKING_DIR/local --without-x --with-drivers=FILES --disable-cups --disable-gtk --without-tesseract --with-libiconv=gnu --disable-threading --disable-byteswap-h --disable-fontconfig --disable-dbus --with-libtiff

ghostscript: ghostscript/Makefile
	cd ghostscript; $(MAKE) -j1; $(MAKE) install

texmacs: local/bin/qmake local/lib/libfreetype.a
	rm -rf texmacs/distr; cd texmacs/src; autoreconf; PKG_CONFIG_PATH=$$WORKING_DIR/local/lib/pkgconfig LIBS="-L/opt/homebrew/lib" TMREPO=$$WORKING_DIR/local ./configure --with-tmrepo=$$WORKING_DIR/local --prefix=$$WORKING_DIR/local --with-guile=embedded18 --disable-fastalloc; $(MAKE); $(MAKE) PACKAGE
