CFLAGS += -MD # Used to generate dependencies
CFLAGS += -pipe -Wall -g3
LDFLAGS += -pthread

vm_obj = vm.o heap.o native.o primitives.o hash.o \
		 module.o strings.o bytevector.o struct.o \
		posix.o const_allocator.o types.o

.PHONY: all clean
all: vm

-include *.d

clean:
	rm -f *.o *.d vm

vm: $(vm_obj)

# FIXME
# Add mzscheme variant
r6rs = ypsilon --sitelib=../scheme

regen-opcode:
	@$(r6rs) ../scheme/gen-headers.scm
