all: qt guile

.PHONY: all qt guile

qt:
	-mkdir qt-build
	cd qt-build; ../qt/configure -developer-build -nomake tests -nomake examples -no-warnings-are-errors -skip qtwebengine -no-opengl; cmake --build . --target host_tools

guile:
	cd guile; ./configure; make