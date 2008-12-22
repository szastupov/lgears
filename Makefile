CFLAGS=-Wall -pipe -ggdb

compiler_obj = compiler.o string_util.o tokenizer.o btree.o memory.o ast.o
vm_obj = vm.o
targets = compiler vm

include Makefile.inc

compiler: $(compiler_obj)
	@$(CC) $^ -o $@

vm: CFLAGS += -pg
vm: $(vm_obj)
	@$(CC) $^ -o $@ -pg
