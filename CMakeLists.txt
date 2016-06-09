cmake_minimum_required(VERSION 3.0)

project(carp-repl)

set(VERSION_MAJOR "0")
set(VERSION_MINOR "0")
set(VERSION_PATCH "1")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

set(SOURCE_DIR src)
include(globfiles.cmake)

find_package(Threads)
find_library(M_LIB m)


find_package(PkgConfig)

pkg_check_modules(LIBFFI libffi)

mark_as_advanced(LIBFFI_INCLUDE_DIR LIBFFI_LIBRARIES )

include_directories(shared ${LIBFFI_INCLUDE_DIR})


add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_h} ${${PROJECT_NAME}_c})

target_link_libraries(${PROJECT_NAME} ${LIBFFI_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${M_LIB} ${CMAKE_DL_LIBS})

set_target_properties(${PROJECT_NAME}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/bin")
