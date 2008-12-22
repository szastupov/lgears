# vim: syntax=make:
all: $(targets)

.deps/%.dep: %.c
	@mkdir -p .deps
	@set -e; rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@; \
		sed -i 's,\($*\)\.o[ :]*,\1.o $@ : ,g' $@;

deps = $(foreach o,$(targets:=_obj),$($(o):%.o=.deps/%.dep))
-include $(deps)

echo-deps:
	@echo $(deps)

clean:
	rm -f *.o .deps/*.dep $(targets)

ctags:
	@ctags *.c *.h
