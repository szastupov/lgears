CFLAGS=-Wall -pipe -ggdb

compiler_obj = main.o string_util.o tokenizer.o btree.o memory.o ast.o
targets = compiler

all: $(targets)

%.dep: %.c
	@set -e; rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@; \
		sed -i 's,\($*\)\.o[ :]*,\1.o $@ : ,g' $@;

-include ${compiler_obj:.o=.dep}

compiler: $(compiler_obj)
	@$(CC) $^ -o $@ $(LDFLAGS)

clean:
	rm -f *.o *.dep $(targets)

ctags:
	@ctags *.c *.h
