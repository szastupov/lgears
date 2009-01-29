CFLAGS += -Wall -ggdb

compiler_obj = compiler.o compiler_internal.o string_util.o tokenizer.o btree.o ast.o
vm_obj = vm.o heap.o primitives.o
targets = compiler vm 

include include.mk

r6rs = ypsilon --sitelib=./scheme

regen-opcode:
	@$(r6rs) ./scheme/gen-header.scm ./opcodes.h
