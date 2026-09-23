###############################################################################
# MODULE     : tests/Makefile
# DESCRIPTION: build and run the C++ unit tests against an autotools build
#
# Usage (after ./configure && make at the top level):
#   make -C tests            # build and run all tests
#   make -C tests run-tt_tools_test
#   make -C tests TM_TEST_FONT_DIR=/path/to/extra/fonts
#
# The CMake build has its own test harness (tests/CMakeLists.txt); this file
# reuses the flags of src/makefile and links every object of the main build
# except the program entry point.
#
# CAVEAT: when the main build was configured without dependency tracking
# (src/Deps only contains a stamp), objects are not recompiled after a header
# changes. Linking such stale objects gives ABI mismatches and crashes. Run
# `make -C tests check-stale` to list objects older than some header, and
# delete them (or run `make clean` at the top level) before rebuilding.
###############################################################################

.DEFAULT_GOAL := all

TOP   := $(abspath ..)
SRC   := $(TOP)/src
BUILD := $(abspath build)

TEST_SRC := $(shell find . -name '*_test.cpp' | sort)
# legacy tests that do not link in this setup (xml_test includes a .cpp file
# that is already part of the main build)
# mac_images_test calls mac_supports and mac_image_size, which mac_images.h
# does not declare in a Qt 6 build
EXCLUDE  := xml_test mac_images_test
TESTS    := $(filter-out $(EXCLUDE),$(patsubst %.cpp,%,$(notdir $(TEST_SRC))))
vpath %_test.cpp $(sort $(dir $(TEST_SRC)))

# flags of the main build, exported by print-var.mk
$(BUILD)/flags.mk: $(SRC)/makefile print-var.mk
	@mkdir -p $(BUILD)
	$(MAKE) -s -C $(SRC) -f makefile -f $(abspath print-var.mk) print-flags > $@
include $(BUILD)/flags.mk

TM_OBJS := $(filter-out %/unix_entrypoint.o %/windows32_entrypoint.o \
                        %/windows64_entrypoint.o, $(wildcard $(SRC)/Objects/*.o))

TEST_LIBS := -framework QtTest

# environment for running the tests
export TEXMACS_PATH      := $(TOP)/TeXmacs
export TEXMACS_HOME_PATH := $(BUILD)/home
export TM_TEST_FONT_DIR

.PHONY: all run build clean check-stale

all: run

build: $(patsubst %,$(BUILD)/%,$(TESTS))

run: build
	@mkdir -p $(TEXMACS_HOME_PATH)
	@status=0; for t in $(TESTS); do \
	  echo "===== $$t"; \
	  $(BUILD)/$$t -silent || status=1; \
	done; exit $$status

run-%: $(BUILD)/%
	@mkdir -p $(TEXMACS_HOME_PATH)
	$< $(ARGS)

# only the tests written against QtTest declare a QObject; the others are
# plain TeXmacs C++ over tests/tm_test.hpp and get an empty stub
$(BUILD)/%.moc: %.cpp
	@mkdir -p $(BUILD)
	@if grep -q Q_OBJECT $<; then 	  $(TM_MOC) $(TM_MOCFLAGS) $(abspath $<) -o $@; 	else : > $@; fi

# test objects depend on every header of the main build: a changed vtable
# in a stale object is a crash, not a compile error
TM_HEADERS := $(shell find $(SRC) -name '*.hpp' -not -path '*/Objects/*')

$(BUILD)/%.o: %.cpp $(BUILD)/%.moc $(TM_HEADERS)
	cd $(SRC) && $(TM_CXX) $(TM_INCL) $(TM_CXXFLAGS) -I$(BUILD) -I$(abspath .) \
	  -c $(abspath $<) -o $@

$(BUILD)/%: $(BUILD)/%.o $(TM_OBJS)
	$(TM_LD) $(TM_OBJS) $< $(TM_LINK_OPTIONS) $(TM_LDFLAGS) $(TM_LIBS) \
	  $(TEST_LIBS) -o $@

clean:
	rm -rf $(BUILD)

# objects of the main build that are older than the newest header
check-stale:
	@newest=$$(find $(SRC) -name '*.hpp' -newer $(SRC)/makefile.in \
	  -exec ls -t {} + 2>/dev/null | head -1); \
	[ -n "$$newest" ] || newest=$$(ls -t $$(find $(SRC) -name '*.hpp') | head -1); \
	echo "newest header: $$newest"; \
	stale=$$(find $(SRC)/Objects -name '*.o' ! -newer "$$newest"); \
	if [ -n "$$stale" ]; then \
	  echo "$$stale" | wc -l | xargs echo "stale objects:"; \
	  echo "remove them with: find $(SRC)/Objects -name '*.o' ! -newer '$$newest' -delete"; \
	else echo "no stale objects"; fi

.PHONY: all build run clean check-stale
.PRECIOUS: $(BUILD)/%.moc $(BUILD)/%.o
