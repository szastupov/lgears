add_executable(vm vm.c heap.c native.c primitives.c hash.c module.c struct.c string.c fd.c bytevector.c fs.c ffi.c types.c const_allocator.c)
target_link_libraries(vm ${CMAKE_THREAD_LIBS_INIT} ${CMAKE_DL_LIBS} ${LIBFFI_LIBRARIES})

install(TARGETS vm
	RUNTIME DESTINATION bin)
