cmake_minimum_required (VERSION 2.8) 
project(scheme)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP") 
#set project output directory
set(EXECUTABLE_OUTPUT_PATH "${PROJECT_OUTPUT_DIR}")
set(LIBRARY_OUTPUT_PATH "${PROJECT_OUTPUT_DIR}")
add_definitions(-DX86_64)
#build setting: just unicode version

include(${PROJECT_SOURCE_DIR}/c/src.cmake)
include_directories("${PROJECT_SOURCE_DIR}/ta6nt/c/")
include_directories("${PROJECT_SOURCE_DIR}/boot/ta6nt/")
include_directories("${PROJECT_BINARY_DIR}/zlib/")
include_directories("${PROJECT_SOURCE_DIR}/zlib/")
subdirs(zlib)
add_executable(scheme ${src})
target_link_libraries(scheme
  zlibstatic
  rpcrt4.lib ole32.lib advapi32.lib User32.lib
  )
  
