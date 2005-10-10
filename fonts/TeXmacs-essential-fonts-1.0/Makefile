
###############################################################################
# MODULE     : Makefile for essential TeXmacs fonts (not in Bluesky)
# BY         : Joris van der Hoeven
# COPYRIGHT  : This software falls under the GNU general public license;
#              see the file 'LICENSE', which is provided with this package.
###############################################################################

MFTRACE = mftrace --autotrace --formats=pfb
MKTEXTFM = mktextfm
PFBDIR = $(PWD)/TeXmacs/fonts/type1
TFMDIR = $(PWD)/TeXmacs/fonts/tfm
MKDIR = mkdir -p
CD = cd
RM = rm -f

all: dirs tfm pfb
	$(RM) *.log

dirs:
	$(MKDIR) TeXmacs
	$(MKDIR) TeXmacs/fonts
	$(MKDIR) $(TFMDIR)
	$(MKDIR) $(TFMDIR)/ec
	$(MKDIR) $(TFMDIR)/la
	$(MKDIR) $(TFMDIR)/math
	$(MKDIR) $(PFBDIR)
	$(MKDIR) $(PFBDIR)/ec
	$(MKDIR) $(PFBDIR)/la
	$(MKDIR) $(PFBDIR)/math

###############################################################################
# Building the tfm files
###############################################################################

ec_root = ecrm ecsl ecti eccc ecbx ecxc ectt ecss
la_root = larm
math_root = bbm
find_series = \
  $(patsubst %,%5$(2),$(1)) $(patsubst %,%6$(2),$(1)) \
  $(patsubst %,%7$(2),$(1)) $(patsubst %,%8$(2),$(1)) \
  $(patsubst %,%9$(2),$(1)) $(patsubst %,%10$(2),$(1)) \
  $(patsubst %,%11$(2),$(1)) $(patsubst %,%12$(2),$(1)) \
  $(patsubst %,%14$(2),$(1)) $(patsubst %,%17$(2),$(1)) \
  $(patsubst %,%20$(2),$(1)) $(patsubst %,%24$(2),$(1))
ec_series := $(call find_series,$(ec_root),)
la_series := $(call find_series,$(la_root),00)
math_series := $(call find_series,$(math_root),)
find_tfm = $(patsubst %,$(TFMDIR)/$(2)/%.tfm,$(1))
ec_tfm := $(call find_tfm,$(ec_series),ec)
la_tfm := $(call find_tfm,$(la_series),la)
math_tfm := $(call find_tfm,$(math_series),math)

$(ec_tfm): $(TFMDIR)/ec/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/ec $*
	$(RM) $(TFMDIR)/ec/$*.600pk

$(la_tfm): $(TFMDIR)/la/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/la $*
	$(RM) $(TFMDIR)/la/$*.600pk

$(math_tfm): $(TFMDIR)/math/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/math $*
	$(RM) $(TFMDIR)/math/$*.600pk

tfm: $(ec_tfm) $(la_tfm) $(math_tfm)

###############################################################################
# Building the pfb files
###############################################################################

ec_target = ecrm10 ecsl10 ecti10 eccc10 ecbx10 ecxc10 ectt10 ecss10
la_target = larm1000
math_target = bbm10

find_pfb = $(patsubst %,$(PFBDIR)/$(2)/%.pfb,$(1))
ec_pfb := $(call find_pfb,$(ec_target),ec)
la_pfb := $(call find_pfb,$(la_target),la)
math_pfb := $(call find_pfb,$(math_target),math)

$(ec_pfb): $(PFBDIR)/ec/%.pfb:
	$(CD) $(PFBDIR)/ec; $(MFTRACE) $*

$(la_pfb): $(PFBDIR)/la/%.pfb:
	$(CD) $(PFBDIR)/la; $(MFTRACE) $*

$(math_pfb): $(PFBDIR)/math/%.pfb:
	$(CD) $(PFBDIR)/math; $(MFTRACE) $*

pfb: $(ec_pfb) $(la_pfb) $(math_pfb)

###############################################################################
# Cleaning
###############################################################################

clean:
	$(RM) *~
	$(RM) */*~
	$(RM) */*/*~
	$(RM) */*/*/*~
	$(RM) $(TFMDIR)/ec/*
	$(RM) $(TFMDIR)/la/*
	$(RM) $(TFMDIR)/math/*
	$(RM) $(PFBDIR)/ec/*
	$(RM) $(PFBDIR)/la/*
	$(RM) $(PFBDIR)/math/*
