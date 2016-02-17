
file(GLOB ${PROJECT_NAME}_mac_c "${SOURCE_DIR}/*_MacOSX.c")
file(GLOB ${PROJECT_NAME}_mac_cpp "${SOURCE_DIR}/*_MacOSX.cpp")
file(GLOB ${PROJECT_NAME}_mac_m "${SOURCE_DIR}/*_MacOSX.m")
file(GLOB ${PROJECT_NAME}_mac_mm "${SOURCE_DIR}/*_MacOSX.mm")
file(GLOB ${PROJECT_NAME}_mac_h "${SOURCE_DIR}/*_MacOSX.h")
list(APPEND ${PROJECT_NAME}_mac_c ${${PROJECT_NAME}_mac_cpp} ${${PROJECT_NAME}_mac_m} ${${PROJECT_NAME}_mac_mm})

file(GLOB ${PROJECT_NAME}_win_c "${SOURCE_DIR}/*_MSWIN.c")
file(GLOB ${PROJECT_NAME}_win_cpp "${SOURCE_DIR}/*_MSWIN.cpp")
file(GLOB ${PROJECT_NAME}_win_h "${SOURCE_DIR}/*_MSWIN.h")
list(APPEND ${PROJECT_NAME}_win_c ${${PROJECT_NAME}_win_cpp})

file(GLOB ${PROJECT_NAME}_linux_c "${SOURCE_DIR}/*_Linux.c")
file(GLOB ${PROJECT_NAME}_linux_cpp "${SOURCE_DIR}/*_Linux.cpp")
file(GLOB ${PROJECT_NAME}_linux_h "${SOURCE_DIR}/*_Linux.h")
list(APPEND ${PROJECT_NAME}_mac_c ${${PROJECT_NAME}_linux_cpp})

file(GLOB ${PROJECT_NAME}_c "${SOURCE_DIR}/*.c")
file(GLOB ${PROJECT_NAME}_cpp "${SOURCE_DIR}/*.cpp")
file(GLOB ${PROJECT_NAME}_h "${SOURCE_DIR}/*.h")
list(APPEND ${PROJECT_NAME}_c ${${PROJECT_NAME}_cpp})

if(${PROJECT_NAME}_mac_c OR ${PROJECT_NAME}_win_c OR ${PROJECT_NAME}_linux_c)
  list(REMOVE_ITEM ${PROJECT_NAME}_c ${${PROJECT_NAME}_mac_c} ${${PROJECT_NAME}_win_c} ${${PROJECT_NAME}_linux_c})

  if(WIN32)
    list(APPEND ${PROJECT_NAME}_c ${${PROJECT_NAME}_win_c})
  elseif(APPLE)
    list(APPEND ${PROJECT_NAME}_c ${${PROJECT_NAME}_mac_c})
  else()
    list(APPEND ${PROJECT_NAME}_c ${${PROJECT_NAME}_linux_c})
  endif()

endif()

if(${PROJECT_NAME}_mac_h OR ${PROJECT_NAME}_win_h OR ${PROJECT_NAME}_linux_h)
  list(REMOVE_ITEM ${PROJECT_NAME}_h ${${PROJECT_NAME}_mac_h} ${${PROJECT_NAME}_win_h} ${${PROJECT_NAME}_linux_h})

  if(WIN32)
    list(APPEND ${PROJECT_NAME}_h ${${PROJECT_NAME}_win_h})
  elseif(APPLE)
    list(APPEND ${PROJECT_NAME}_h ${${PROJECT_NAME}_mac_h})
  else()
    list(APPEND ${PROJECT_NAME}_h ${${PROJECT_NAME}_linux_h})
  endif()

endif()
