include(FindThreads)
add_definitions(-Wall)
set(CMAKE_C_FLAGS_DEBUG "-g3")
set(CMAKE_C_FLAGS_RELEASE "-O3")

add_executable(vm vm.c heap.c native.c primitives.c hash.c module.c vector.c string.c fd.c bytevector.c)
target_link_libraries(vm ${CMAKE_THREAD_LIBS_INIT})

install(TARGETS vm
	RUNTIME DESTINATION bin)
