CFLAGS=-Wall -ggdb

compiler_obj = compiler.o string_util.o tokenizer.o btree.o ast.o
vm_obj = vm.o
targets = compiler vm

vm: CFLAGS += -pg
vm: LDFLAGS += -pg

include Makefile.inc
