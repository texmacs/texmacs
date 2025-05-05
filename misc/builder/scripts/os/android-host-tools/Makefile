all: qt guile

.PHONY: all qt guile

qt:
	-mkdir qt-build
	-mkdir qt-build/bin
	cp patched/QtPkgConfigHelpers.cmake qt/qtbase/cmake/QtPkgConfigHelpers.cmake
	cd qt-build; ../qt/configure -developer-build -nomake tests -nomake examples -no-warnings-are-errors -skip qtwebengine -no-opengl; cmake --build . --target host_tools --parallel
	cp qt-build/qtbase/bin/androiddeployqt qt-build/bin/androiddeployqt
	cp qt-build/qtbase/bin/qmake qt-build/qtbase/bin/qmake6

guile:
	cd guile; ./configure --prefix=$$HOST_WORKING_DIR/local; make; make install
