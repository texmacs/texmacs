all: local/lib/libltdl.a local/lib/libpng16.a local/lib/libfreetype.a local/lib/libgmp.a local/lib/libguile.a local/bin/qmake texmacs

.PHONY: texmacs 

libtool/Makefile:
	cd libtool; emconfigure ./configure --prefix=$$WORKING_DIR/local 

libtool/libltdl/.libs/libltdl.a: libtool/Makefile
	cd libtool; emmake $(MAKE) 

local/lib/libltdl.a: libtool/libltdl/.libs/libltdl.a
	cd libtool; emmake $(MAKE) install
	if [ -f "local/lib/libltdl.a" ]; then echo "libltdl.a has been installed"; else echo "error on libltdl"; exit; fi;

# libpng freetype are provided by emscripten

local/bin/qmake:
# build the wasm version of Qt and put it in $WORKING_DIR/local
# - need to have Qt 6.9 sources in $HOST_WORDKING_DIR/qt
# - needs emscripten version 3.1.70, see here for instructions: https://emscripten.org/docs/getting_started/downloads.html
# - reset the devel paths with set-devel-paths-wasm
	-rm -rf qt-build
	-mkdir qt-build
	cd qt-build && $$HOST_WORDKING_DIR/qt/configure \
		-opensource -confirm-license -prefix $$WORKING_DIR/local -release \
		-qt-host-path $$HOST_WORKING_DIR/local -platform wasm-emscripten \
		-nomake tests -nomake examples -no-warnings-are-errors -pkg-config \
		-skip qt3d \
		-skip qtactiveqt \
		-skip qtcharts \
		-skip qtcoap \
		-skip qtdatavis3d \
		-skip qtdeclarative \
		-skip qtdoc \
		-skip qtgamepad \
		-skip qtgraphicaleffects \
		-skip qtimageformats \
		-skip qtlocation \
		-skip qtlottie \
		-skip qtmultimedia \
		-skip qtnetworkauth \
		-skip qtopcua \
		-skip qtpositioning \
		-skip qtquick3d \
		-skip qtquickcontrols \
		-skip qtquickcontrols2 \
		-skip qtremoteobjects \
		-skip qtscript \
		-skip qtscxml \
		-skip qtsensors \
		-skip qtserialbus \
		-skip qtserialport \
		-skip qtspeech \
		-skip qttools \
		-skip qttranslations \
		-skip qtvirtualkeyboard \
		-skip qtwayland \
		-skip qtwebchannel \
		-skip qtwebengine \
		-skip qtwebsockets \
		-skip qtwebview \
		-skip qtx11extras \
		-skip qtxmlpatterns \
		-skip qtgraphs \
		-skip qtmqtt \
		-skip qtquick3dphysics \
		-skip qtquickeffectmaker \
		-skip qtquicktimeline \
		-- \
		-G Ninja && \
		cmake --build . --parallel && \
		cmake --build . --target install --parallel

texmacs:
	-cd texmacs/src; make -i distclean
	-rm -rf texmacs/distr;
	cp -R local/share/guile-texmacs/1.8/* texmacs/src/TeXmacs/progs/
	cd texmacs/src; autoreconf; ./configure --host=$$TARGET --with-texmacs-builder=$$WORKING_DIR/local --prefix=$$WORKING_DIR/local --disable-gs --with-gnu-ld=yes --with-qt-find-method=pkgconfig --disable-gs; $(MAKE) ANDROID_DEV_APK
