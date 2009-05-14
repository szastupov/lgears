project(lgears)
cmake_minimum_required(VERSION 2.6)

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
