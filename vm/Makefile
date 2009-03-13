CFLAGS += -Wall -g3 -DCOMPUTED_GOTO -pg
LDFLAGS += -pg -pthread

vm_obj = vm.o heap.o primitives.o hash.o module.o fixnum.o
targets = vm

include include.mk

# FIXME
# Add mzscheme variant
r6rs = ypsilon --sitelib=../scheme

regen-opcode:
	@$(r6rs) ../scheme/gen-headers.scm
