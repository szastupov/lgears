CFLAGS=-Wall -pipe -ggdb

compiler_obj = compiler.o string_util.o tokenizer.o btree.o memory.o ast.o
vm_obj = vm.o

targets = compiler vm

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

compiler: $(compiler_obj)
	@$(CC) $^ -o $@ $(LDFLAGS)

vm: $(vm_obj)
	@$(CC) $^ -o $@ $(LDFLAGS)

clean:
	rm -f *.o .deps/*.dep $(targets)

ctags:
	@ctags *.c *.h
