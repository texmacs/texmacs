$(texmacs_deps): Deps/%.d: %.cpp

$(deps):
	@ echo "make deps: $@"
	@ set -e; $(CXX) -M $(deps_flags) $< \
	  | sed 's|\($*\)\.o[ :]*|Objects/\1.o $@ : |g' > $@; \
	  [ -s $@ ] || $(RM) $@

# To avoid unneeded processing, the Deps will not be included (and thus
# remade if needed) unless the "deps" target has been previously made,
# by hand.

deps: $(deps)
	$(TOUCH) Deps/stamp

.PHONY: deps

ifeq ($(shell test -f Deps/stamp && echo yes || echo no),yes)
include $(deps)
endif
