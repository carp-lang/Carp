cmake_minimum_required(VERSION 3.0)

project(carp-repl)

set(VERSION_MAJOR "0")
set(VERSION_MINOR "0")
set(VERSION_PATCH "1")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

# Turn on optimizations, but turn off strict aliasing
set(CMAKE_C_FLAGS  "${CMAKE_CXX_FLAGS} -O3 -fno-strict-aliasing")

set(SOURCE_DIR src)
include(globfiles.cmake)

# Add executable
add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_h} ${${PROJECT_NAME}_c})

# Add appropriate thread library
find_package(Threads)

# Math library for sqrt, etc.
find_library(M_LIB m)

# Find libffi using pkg-config
find_package(PkgConfig)
pkg_check_modules(LIBFFI libffi)
mark_as_advanced(LIBFFI_INCLUDE_DIR LIBFFI_LIBRARIES )
include_directories(shared ${LIBFFI_INCLUDE_DIR})

# Add libraries to the target
target_link_libraries(${PROJECT_NAME} ${LIBFFI_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${M_LIB} ${CMAKE_DL_LIBS})


set_target_properties(${PROJECT_NAME}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/bin")
