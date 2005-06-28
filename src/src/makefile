
###############################################################################
# MODULE     : main make file for TeXmacs source
# BY         : Joris van der Hoeven, David Allouche
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

default: all

main_src:=.
include common.makefile
include deps.makefile

###############################################################################
# Main makes
###############################################################################

vpath %.cpp $(shell find . -type d '!' -name Deps '!' -name Objects|tr '\n' ' ')

all: $(tmsrc)/bin/texmacs.bin
clean: CLEAN

.PHONY: all clean

###############################################################################
# Make editor
###############################################################################

# Compilation rules

TEXMACS_objects := $(strip $(call uses,system classes data resource plugins\
 window typeset edit glue) $(texmacs_obj) $(instanciations_obj))

link_options:=-L$(tmsrc)/lib -lm $(LDBPATH) $(LDBXWINDOW) \
  $(LDBGUILE) $(LDBIMLIB2) $(LDBFREETYPE) $(LDBICONV) $(LDBDL)

$(tmsrc)/bin/texmacs.bin: Objects/texmacs-$(link).bin
	$(CP) $^ $@

Objects/texmacs-$(link).bin: $(TEXMACS_objects)
	$(call link_bin,$(link_options))

.PHONY: $(tmsrc)/bin/texmacs.bin

###############################################################################
# Interface for Guile
###############################################################################

gluesrc = Guile/Glue

GLUE: $(gluesrc)/glue_basic.cpp $(gluesrc)/glue_editor.cpp $(gluesrc)/glue_server.cpp

.PHONY: GLUE

$(gluesrc)/glue_basic.cpp: $(gluesrc)/build-glue.scm
$(gluesrc)/glue_editor.cpp: $(gluesrc)/build-glue.scm 
$(gluesrc)/glue_server.cpp: $(gluesrc)/build-glue.scm

$(gluesrc)/glue_basic.cpp: $(gluesrc)/build-glue-basic.scm
	$(GLUE) build-glue-basic.scm glue_basic.cpp

$(gluesrc)/glue_editor.cpp: $(gluesrc)/build-glue-editor.scm
	$(GLUE) build-glue-editor.scm glue_editor.cpp

$(gluesrc)/glue_server.cpp: $(gluesrc)/build-glue-server.scm
	$(GLUE) build-glue-server.scm glue_server.cpp

###############################################################################
# Cleaning and backup
###############################################################################

DISTR: 
	$(RM) Deps/*.*
	$(RM) Deps/stamp

CLEAN: DISTR
	$(RM) Objects/*.o

maintainer-clean: CLEAN
	$(RM) Objects/*.*

PHONY: DISTR CLEAN maintainer-clean
