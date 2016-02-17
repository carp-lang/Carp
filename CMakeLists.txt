cmake_minimum_required(VERSION 3.0)

project(carp-repl)

set(VERSION_MAJOR "0")
set(VERSION_MINOR "0")
set(VERSION_PATCH "1")
set(VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

set(SOURCE_DIR src)
include(globfiles.cmake)

# list(APPEND SOURCE_FILES src/main.c src/obj.c src/gc.c src/obj_string.c src/reader.c src/eval.c src/env.c src/primops.c src/repl.c)

# ${SOURCE_FILES}

include_directories(/usr/local/opt/libffi/lib/libffi-3.0.13/include)
link_directories(/usr/local/opt/libffi/lib/)

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_h} ${${PROJECT_NAME}_c})

target_link_libraries(${PROJECT_NAME} ffi)

set_target_properties(${PROJECT_NAME}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/bin"
    )
  
