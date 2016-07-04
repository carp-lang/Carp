cmake_minimum_required(VERSION 3.0)

project(carp-repl)

set(VERSION_MAJOR "0")
set(VERSION_MINOR "0")
set(VERSION_PATCH "1")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_GNU_SOURCE")
endif()

set(SOURCE_DIR src)
include(globfiles.cmake)

include(FindPkgConfig)
PKG_SEARCH_MODULE(LIBFFI REQUIRED libffi)

include_directories(
    ${LIBFFI_INCLUDE_DIRS})

link_directories(
    ${LIBFFI_LIBRARIES})

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_h} ${${PROJECT_NAME}_c})

target_link_libraries(${PROJECT_NAME}
    ${LIBFFI_LIBRARIES}
    m
    c
    pthread
    dl)

set_target_properties(${PROJECT_NAME}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/bin")

add_custom_target(
    format
    COMMAND find . -iname '*.[ch]' | xargs clang-format -style=file -i
)

