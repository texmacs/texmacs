
###############################################################################
# MODULE     : Makefile for the main TeXmacs fonts which are not in Bluesky
#              and not in the essential font package
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
	$(MKDIR) $(TFMDIR)/tc
	$(MKDIR) $(PFBDIR)
	$(MKDIR) $(PFBDIR)/ec
	$(MKDIR) $(PFBDIR)/la
	$(MKDIR) $(PFBDIR)/math
	$(MKDIR) $(PFBDIR)/tc

###############################################################################
# Building the tfm files
###############################################################################

#ec_root = ecrm ecsl ecti eccc ecbx ecxc ectt ecss
ec_root = ecdh ecbl ecbi ecrb ecoc ecst ectc ecvt ecvi ecsi ecsx ecso ecss \
  eorm eosl eoti eocc
#la_root = larm
la_root = lasl lati lacc ladh labx labl labi laxc \
  latt last lass lasi lasx laso
#math_root = bbm
math_root = bbmsl bbmbx bbmbxsl bbmss grmn grxn euxm \
  bbold dsrom dsss
tc_root = tcrm tcsl tcbx tcbi tcss tcsx tctt
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
tc_series := $(call find_series,$(tc_root),)
find_tfm = $(patsubst %,$(TFMDIR)/$(2)/%.tfm,$(1))
ec_tfm := $(call find_tfm,$(ec_series),ec)
la_tfm := $(call find_tfm,$(la_series),la)
math_tfm := $(call find_tfm,$(math_series),math)
tc_tfm := $(call find_tfm,$(tc_series),tc)

$(ec_tfm): $(TFMDIR)/ec/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/ec $*
	$(RM) $(TFMDIR)/ec/$*.600pk

$(la_tfm): $(TFMDIR)/la/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/la $*
	$(RM) $(TFMDIR)/la/$*.600pk

$(math_tfm): $(TFMDIR)/math/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/math $*
	$(RM) $(TFMDIR)/math/$*.600pk

$(tc_tfm): $(TFMDIR)/tc/%.tfm:
	$(MKTEXTFM) --destdir $(TFMDIR)/tc $*
	$(RM) $(TFMDIR)/tc/$*.600pk

tfm: $(ec_tfm) $(la_tfm) $(math_tfm) $(tc_tfm)

###############################################################################
# Building the pfb files
###############################################################################

#ec_target = ecrm10 ecsl10 ecti10 eccc10 ecbx10 ecxc10 ectt10 ecss10
ec_target = \
  ecrm5 ecrm7 ecrm12 ecrm17 ecsl7 ecsl12 \
  ecti7 ecti12 ecff10 eccc7 eccc12 ecdh10 \
  ecbx7 ecbx12 ecbx17 ecbl10 ecbi10 ecrb10 \
  ecxc12 ecxc17 ecoc10 \
  ectt7 ectt12 ecst10 ectc10 ecvt10 ecvi10 \
  ecss7 ecss12 ecsi10 ecsx10 ecso10 ecssdc10 \
  eorm10 eosl10 eoti10 eocc10

#la_target = larm1000
la_target = \
  larm700 lasl1000 lati1000 lacc1000 ladh1000 \
  labx1000 labx1700 labl1000 labi1000 laxc1000 \
  latt1000 last1000 \
  lass1000 lasi1000 lasx1000 laso1000

#math_target = bbm10
math_target = \
  bbm7 bbmsl10 bbmbx7 bbmbx10 bbmbxsl10 bbmss10 \
  grmn10 grxn10 \
  euxm7 euxm10 \
  bbold7 bbold10 dsrom8 dsrom10 dsss8 dsss10

tc_target = \
  tcrm10 tcsl10 tcbx10 tcbi10 tcss10 tcsx10 tctt10

find_pfb = $(patsubst %,$(PFBDIR)/$(2)/%.pfb,$(1))
ec_pfb := $(call find_pfb,$(ec_target),ec)
la_pfb := $(call find_pfb,$(la_target),la)
math_pfb := $(call find_pfb,$(math_target),math)
tc_pfb := $(call find_pfb,$(tc_target),tc)

$(ec_pfb): $(PFBDIR)/ec/%.pfb:
	$(CD) $(PFBDIR)/ec; $(MFTRACE) $*

$(la_pfb): $(PFBDIR)/la/%.pfb:
	$(CD) $(PFBDIR)/la; $(MFTRACE) $*

$(math_pfb): $(PFBDIR)/math/%.pfb:
	$(CD) $(PFBDIR)/math; $(MFTRACE) $*

$(tc_pfb): $(PFBDIR)/tc/%.pfb:
	$(CD) $(PFBDIR)/tc; $(MFTRACE) $*

pfb: $(ec_pfb) $(la_pfb) $(math_pfb) $(tc_pfb)

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
	$(RM) $(TFMDIR)/tc/*
	$(RM) $(PFBDIR)/ec/*
	$(RM) $(PFBDIR)/la/*
	$(RM) $(PFBDIR)/math/*
	$(RM) $(PFBDIR)/tc/*
