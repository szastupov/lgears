CFLAGS = -Wall -ggdb

compiler_obj = compiler.o compiler_internal.o string_util.o tokenizer.o btree.o ast.o
vm_obj = vm.o heap.o
#lexer_obj = lexer.o
targets = compiler vm 

vm: CFLAGS += -pg
vm: LDFLAGS += -pg

include include.mk
