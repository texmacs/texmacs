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
###############################################################################

TOP   := $(abspath ..)
SRC   := $(TOP)/src
BUILD := $(abspath build)

TEST_SRC := $(shell find . -name '*_test.cpp' | sort)
# legacy tests that do not link in this setup (xml_test includes a .cpp file
# that is already part of the main build)
EXCLUDE  := xml_test
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

$(BUILD)/%.moc: %.cpp
	@mkdir -p $(BUILD)
	$(TM_MOC) $(TM_MOCFLAGS) $(abspath $<) -o $@

$(BUILD)/%.o: %.cpp $(BUILD)/%.moc
	cd $(SRC) && $(TM_CXX) $(TM_INCL) $(TM_CXXFLAGS) -I$(BUILD) \
	  -c $(abspath $<) -o $@

$(BUILD)/%: $(BUILD)/%.o $(TM_OBJS)
	$(TM_LD) $(TM_OBJS) $< $(TM_LINK_OPTIONS) $(TM_LDFLAGS) $(TM_LIBS) \
	  $(TEST_LIBS) -o $@

clean:
	rm -rf $(BUILD)

.PHONY: all build run clean
.PRECIOUS: $(BUILD)/%.moc $(BUILD)/%.o
