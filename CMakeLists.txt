project(lgears)
cmake_minimum_required(VERSION 2.6)

include(FindThreads)
include(FindPkgConfig)

option(FATAL_SAFE_ASSERT "Die when safe assertion failed" ON)
option(DEBUG_TRACE_OPCODE "Trace opcode" OFF)
option(COMPUTED_GOTO "Use 'threaded' code" ON)
option(CHECK_OPSTACK "Check operand stack for overflow" ON)

pkg_check_modules(LIBFFI libffi)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/config.h)

add_definitions(-Wall)
set(CMAKE_C_FLAGS_DEBUG "-g3")
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(vm)

install(DIRECTORY scheme
	DESTINATION share/lgears)

# Quck'n' dirty generation of packages
set(CPACK_PACKAGE_CONTACT "redchrom@gmail.com")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Just another Scheme")
set(CPACK_GENERATOR "DEB;RPM;TGZ")
set(CPACK_DEBIAN_PACKAGE_SECTION "interpreters")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.7)")
include(CPack)
