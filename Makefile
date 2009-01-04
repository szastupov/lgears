CFLAGS = -Wall -ggdb

compiler_obj = compiler.o compiler_internal.o string_util.o tokenizer.o btree.o ast.o
vm_obj = vm.o
#lexer_obj = lexer.o
heap_obj = heap.o
targets = compiler vm heap# lexer

vm: CFLAGS += -pg
vm: LDFLAGS += -pg

include include.mk
