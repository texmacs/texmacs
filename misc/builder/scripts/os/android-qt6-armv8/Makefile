all: local/lib/libltdl.a local/lib/libpng16.a local/lib/libfreetype.a local/lib/libgmp.a local/lib/libguile.a local/bin/qmake local/lib/libgnutls.a texmacs

.PHONY: texmacs local/bin/qmake

libtool/Makefile:
	cd libtool; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --disable-silent-rules --enable-static --disable-shared --with-gnu-ld=yes

libtool/libltdl/.libs/libltdl.a: libtool/Makefile
	cd libtool; $(MAKE)

local/lib/libltdl.a: libtool/libltdl/.libs/libltdl.a
	cd libtool; $(MAKE) install
	if [ -f "local/lib/libltdl.a" ]; then echo "libltdl.a has been installed"; else echo "error on libltdl"; exit; fi;

libpng/Makefile:
	cd libpng; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes

libpng/.libs/libpng16.a: libpng/Makefile
	cd libpng; $(MAKE)

local/lib/libpng16.a: libpng/.libs/libpng16.a
	cd libpng; $(MAKE) install
	if [ -f "local/lib/libpng16.a" ]; then echo "libpng16.a has been installed"; else echo "error on libpng"; exit; fi;

freetype/build/Makefile: local/lib/libpng16.a
	cd freetype; mkdir -p build; cd build; ../configure --host=$$TARGET --prefix=$$WORKING_DIR/local --with-gnu-ld=yes --with-bzip2=no --with-harfbuzz=no --with-brotli=no --enable-static=no

freetype/build/libfreetype.a: freetype/build/Makefile
	cd freetype/build; $(MAKE)

local/lib/libfreetype.a: freetype/build/libfreetype.a
	cd freetype/build; make install
	if [ -f "local/lib/libfreetype.a" ]; then echo "libfreetype.a has been installed"; else echo "error on libfreetype"; exit; fi;

gmp/build/Makefile:
	cd gmp; mkdir -p build; cd build; ../configure --host=$$TARGET --enable-assembly=no --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-gnu-ld=yes

gmp/build/.libs/libgmp.a: gmp/build/Makefile
	cd gmp/build; $(MAKE)

local/lib/libgmp.a: gmp/build/.libs/libgmp.a
	cd gmp/build; $(MAKE) install

nettle/Makefile: local/lib/libltdl.a local/lib/libgmp.a
	cd nettle; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared

nettle/lib/.libs/libnettle.a: nettle/Makefile
	cd nettle; $(MAKE)

local/lib/libnettle.a: nettle/lib/.libs/libnettle.a
	cd nettle; $(MAKE) install
	if [ -f "local/lib/libnettle.a" ]; then echo "libnettle.a has been installed"; else echo "error on libnettle"; exit; fi;

brotli/build/Makefile:
	mkdir -p brotli/build
	cd brotli/build && \
	cmake .. \
		-DCMAKE_TOOLCHAIN_FILE=$$NDK/build/cmake/android.toolchain.cmake \
		-DCMAKE_BUILD_TYPE=Release \
		-DCMAKE_INSTALL_PREFIX=$$WORKING_DIR/local \
		-DCMAKE_SYSTEM_NAME=Android \
		-DCMAKE_SYSTEM_VERSION=$$ANDROID_API \
		-DCMAKE_ANDROID_ARCH_ABI=$$ANDROID_ABI \
		-DCMAKE_ANDROID_NDK=$$NDK \
		-DBUILD_SHARED_LIBS=OFF \
		-DCMAKE_POSITION_INDEPENDENT_CODE=ON

brotli/build/libbrotlicommon.a: brotli/build/Makefile
	cd brotli/build; $(MAKE)

local/lib/libbrotlicommon.a: brotli/build/libbrotlicommon.a
	cd brotli/build; $(MAKE) install
	if [ -f "local/lib/libbrotlicommon.a" ]; then echo "brotli has been installed"; else echo "error on brotli"; exit; fi;

gnutls/Makefile: local/lib/libltdl.a local/lib/libgmp.a local/lib/libnettle.a
	cd gnutls; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared --with-included-libtasn1 --with-included-unistring --without-p11-kit --disable-guile --without-brotli --disable-hardware-acceleration

gnutls/lib/.libs/libgnutls.a: gnutls/Makefile
	cd gnutls; $(MAKE)

local/lib/libgnutls.a: gnutls/lib/.libs/libgnutls.a
	cd gnutls; $(MAKE) install
	if [ -f "local/lib/libgnutls.a" ]; then echo "libgnutls.a has been installed"; else echo "error on libgnutls"; exit; fi;

guile/Makefile: local/lib/libgmp.a
	cd guile; rm ./doc/r5rs/r5rs.texi; touch ./doc/r5rs/r5rs.texi;
	cd guile; autoreconf; ./configure --host=$$TARGET --prefix=$$WORKING_DIR/local --enable-static --disable-shared --disable-error-on-warning --disable-networking --disable-nls --enable-debug-freelist --enable-debug-malloc --enable-guile-debug --with-threads=no --with-gnu-ld=yes

guile/libguile/.libs/libguile.a: guile/Makefile
	-cd guile; $(MAKE)

local/lib/libguile.a: guile/libguile/.libs/libguile.a
	cd guile; $(MAKE) install

local/bin/qmake:
	-find local -name "*qt*" -exec rm -rf {} \;
	-find local -name "*Qt*" -exec rm -rf {} \;
	-mkdir qt-android
	cd qt-android; $$WORKING_DIR/../android-host-tools-x86_64/qt/configure -pkg-config -opensource -confirm-license -xplatform android-clang -qt-host-path $$QT_HOST_PATH/qtbase -prefix $$WORKING_DIR/local -android-ndk $$NDK -android-sdk $$SDK -android-ndk-platform $$ANDROID_API -android-abis $$ANDROID_ABI -release -nomake examples -nomake tests -no-warnings-are-errors -opensource -confirm-license -prefix $$WORKING_DIR/local -release -nomake tests -nomake examples -no-warnings-are-errors -skip qtwebengine -skip qtopcua $$qtopt -- -DQT_FEATURE_brotli=OFF && cmake --build . --parallel && cmake --build . --target install --parallel
	python3 fixpkgconfig.py
	# -mkdir qt6-host/bin
	# todo cp qt6-host/qtbase/bin/androiddeployqt qt6-host/bin/androiddeployqt
	cp $$QT_HOST_PATH/qtbase/libexec/* local/bin/

texmacs:
	-cd texmacs/src; make -i distclean
	-rm -rf texmacs/distr;
	cp -R local/share/guile-texmacs/1.8/* texmacs/src/TeXmacs/progs/
	cd texmacs/src; autoreconf; ./configure --host=$$TARGET --with-texmacs-builder=$$WORKING_DIR/local --prefix=$$WORKING_DIR/local --disable-gs --with-gnu-ld=yes --with-qt-find-method=pkgconfig --disable-gs --with-gnutls=yes; $(MAKE) ANDROID_DEV_APK
