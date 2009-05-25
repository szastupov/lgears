CFLAGS += -Wall -g3
LDFLAGS += -pthread -lffi -ldl

vm_obj = vm.o heap.o native.o primitives.o hash.o \
		 module.o strings.o fd.o bytevector.o struct.o \
		fs.o foreign.o const_allocator.o types.o
programs = vm

ifdef JARI
	JARI_LIBDIR ?= /usr/local/jos
	CFLAGS += -fomit-frame-pointer -nostdinc -nostdlib -fno-builtin	\
		-I$(JARI_LIBDIR)/include -DJARI
	LDFLAGS = -nostdlib -Wl,-z,combreloc,-T,$(JARI_LIBDIR)/lib/ejo.ld \
		-Wl,-static,-e,__entry_point
	LIBS = -L$(JARI_LIBDIR)/lib -lpthread -lsys -lc
endif

include include.mk

# FIXME
# Add mzscheme variant
r6rs = ypsilon --sitelib=../scheme

regen-opcode:
	@$(r6rs) ../scheme/gen-headers.scm
