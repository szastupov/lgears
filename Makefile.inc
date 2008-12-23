# vim: syntax=make:
CFLAGS +=-pipe

.PHONY: clean echo-deps ctags all

all: $(targets)

.deps/%.dep: %.c
	@mkdir -p .deps
	@set -e; rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@; \
		sed -i 's,\($*\)\.o[ :]*,\1.o $@ : ,g' $@;

deps = $(foreach o,$(targets:=_obj),$($(o):%.o=.deps/%.dep))
-include $(deps)

define target_template
$(1): $$($(1)_obj)
all_objs += $$($(1)_obj)
endef

$(foreach prog,$(targets),$(eval $(call target_template,$(prog))))

$(targets):
	$(LINK.o) $^ -o $@

echo-deps:
	@echo $(deps)

clean:
	rm -f $(all_objs) .deps/*.dep $(targets) gmon.out

ctags:
	@ctags *.c *.h
