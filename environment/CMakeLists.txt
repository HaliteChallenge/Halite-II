cmake_minimum_required(VERSION 2.8)

project(environment)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O2 -Wall -Wno-sign-compare -Wno-unused-function -pedantic")

# versions of cmake before 3.4 always link with -rdynamic on linux, which breaks static linkage with clang
# unfortunately travis right now only has cmake 3.2, so have to do this workaround for now
set(CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "")
set(CMAKE_SHARED_LIBRARY_LINK_CXX_FLAGS "")

include_directories(${CMAKE_SOURCE_DIR}/core)
include_directories(${CMAKE_SOURCE_DIR}/core/mapgen)
include_directories(${CMAKE_SOURCE_DIR}/networking)
include_directories(${CMAKE_SOURCE_DIR}/tclap)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib/common)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib/compress)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib/decompress)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib/deprecated)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib/dictBuilder)
include_directories(${CMAKE_SOURCE_DIR}/zstd-1.3.0/lib/legacy)

get_property(dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)

add_custom_target(VERSION_CHECK
    ./set_version.sh
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

foreach(dir ${dirs})
    file(GLOB_RECURSE SOURCE ${dir}/*.[ch]*)
    set(SOURCE_FILES "${SOURCE_FILES}" ${SOURCE})
endforeach()

include_directories(${CMAKE_SOURCE_DIR})
set(SOURCE_FILES "${SOURCE_FILES}" main.cpp)

add_executable(halite ${SOURCE_FILES})

add_dependencies(halite VERSION_CHECK)

if (APPLE)
    # No static linkage here - https://stackoverflow.com/questions/5259249/creating-static-mac-os-x-c-build
    target_link_libraries(halite pthread)
else()
    # On Linux, make sure to link using the whole-archive option
    # See: https://stackoverflow.com/questions/35116327
    target_link_libraries(halite -Wl,--whole-archive pthread -Wl,--no-whole-archive -static)
endif()
