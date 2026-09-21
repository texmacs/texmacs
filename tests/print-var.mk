# Helper included next to src/makefile to export its variables to tests/Makefile
print-flags:
	@echo 'TM_CXX := $(CXX)'
	@echo 'TM_LD := $(LD)'
	@echo 'TM_INCL := $(deps_flags)'
	@echo 'TM_CXXFLAGS := $(CXXFLAGS)'
	@echo 'TM_LDFLAGS := $(LDFLAGS)'
	@echo 'TM_LIBS := $(LIBS)'
	@echo 'TM_LINK_OPTIONS := $(link_options)'
	@echo 'TM_MOC := $(MOC)'
	@echo 'TM_MOCFLAGS := $(MOCFLAGS)'
