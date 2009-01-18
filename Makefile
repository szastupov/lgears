CFLAGS += -Wall -ggdb

compiler_obj = compiler.o compiler_internal.o string_util.o tokenizer.o btree.o ast.o
vm_obj = vm.o heap.o
targets = compiler vm 

#vm: CFLAGS += -O3

include include.mk
