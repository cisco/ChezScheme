cmake_minimum_required (VERSION 2.8) 
project(scheme)

# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP") 
#set project output directory
set(EXECUTABLE_OUTPUT_PATH "${PROJECT_OUTPUT_DIR}")
set(LIBRARY_OUTPUT_PATH "${PROJECT_OUTPUT_DIR}")
add_definitions(-DX86_64)
#build setting: just unicode version
include(${PROJECT_SOURCE_DIR}/c/src.cmake)
# include_directories("${PROJECT_BINARY_DIR}/zlib/")
# include_directories("${PROJECT_SOURCE_DIR}/zlib/")

# subdirs(zlib)

if(IOS)
  message("handle for ios")
  # SET(CMAKE_CXX_FLAGS " -std=c++11 -fobjc-arc -mfpu=neon  ${CMAKE_CXX_FLAGS} ")
  # set(LIB_PATH_PREFIX "${CMAKE_SOURCE_DIR}/libs/ios")
  # ADD_DEFINITIONS(-DWEBRTC_POSIX)
  # ADD_DEFINITIONS(-DWEBRTC_IOS)
  # ADD_DEFINITIONS(-DWEBRTC_MAC)
  # # set( CMAKE_CXX_FLAGS_RELEASE   "${CMAKE_CXX_FLAGS_RELEASE} -std=c++11 -mfpu=neon")
  # SET(CMAKE_C_FLAGS "-fobjc-arc ${CMAKE_C_FLAGS}")
elseif(APPLE)
  message("handle for mac")
  
  find_package( ZLIB REQUIRED)
  if ( ZLIB_FOUND )
    include_directories( ${ZLIB_INCLUDE_DIRS} )
  endif( ZLIB_FOUND )

  # set(LIB_PATH_PREFIX "${CMAKE_SOURCE_DIR}/libs/mac")
  # SET(CMAKE_CXX_FLAGS " -std=c++11 -msse2  ${CMAKE_CXX_FLAGS} ")
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m64 -Wpointer-arith -Wall -Wextra -Werror -O2 ")
  ADD_DEFINITIONS(-Dapplec)
  ADD_DEFINITIONS(-DSTDC)
  # ADD_DEFINITIONS(-DWEBRTC_POSIX)
  include_directories("${PROJECT_SOURCE_DIR}/a6osx/c/")
  include_directories("/opt/X11/include/")
  include_directories("${PROJECT_SOURCE_DIR}/boot/a6osx/")
  add_executable(scheme ${mac_src})
  target_link_libraries(scheme
    ${ZLIB_LIBRARIES}
    iconv
    m
    ncurses
    )
elseif(ANDROID)
  # SET(CMAKE_CXX_FLAGS " -std=c++11 -mfpu=neon -v ${CMAKE_CXX_FLAGS} ")
  # # set( CMAKE_CXX_FLAGS_RELEASE   "${CMAKE_CXX_FLAGS_RELEASE} -std=c++11 -mfpu=neon")
  # SET(CMAKE_C_FLAGS " -mfpu=neon  ${CMAKE_C_FLAGS}")
else(WIN32)
  include_directories("${PROJECT_SOURCE_DIR}/ta6nt/c/")
  include_directories("${PROJECT_SOURCE_DIR}/boot/ta6nt/")
  add_executable(scheme ${src})
  target_link_libraries(scheme
    zlibstatic
    rpcrt4.lib ole32.lib advapi32.lib User32.lib
    )
  # SET(CMAKE_CXX_FLAGS " -std=c++11 -msse2  ${CMAKE_CXX_FLAGS} ")
  # SET(CMAKE_C_FLAGS " -msse2 ${CMAKE_C_FLAGS}")
endif()




