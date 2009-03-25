CFLAGS += -Wall -g3 -DCOMPUTED_GOTO
LDFLAGS += -pthread

vm_obj = vm.o heap.o native.o primitives.o hash.o module.o fixnum.o vector.o string.o
targets = vm

include include.mk

# FIXME
# Add mzscheme variant
r6rs = ypsilon --sitelib=../scheme

regen-opcode:
	@$(r6rs) ../scheme/gen-headers.scm
