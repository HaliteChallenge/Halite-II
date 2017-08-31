cmake_minimum_required(VERSION 3.7)
project(environment)
set(CMAKE_CXX_STANDARD 11)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -pthread -I ./ -g -O2 -Wall -pedantic")

include_directories(${CMAKE_SOURCE_DIR}/core)
include_directories(${CMAKE_SOURCE_DIR}/core/mapgen)
include_directories(${CMAKE_SOURCE_DIR}/miniz)
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

foreach(dir ${dirs})
    file(GLOB_RECURSE SOURCE ${dir}/*.[ch]*)
    set(SOURCE_FILES "${SOURCE_FILES}" ${SOURCE})
endforeach()

include_directories(${CMAKE_SOURCE_DIR})
set(SOURCE_FILES "${SOURCE_FILES}" main.cpp)

add_executable(halite ${SOURCE_FILES})

TARGET_LINK_LIBRARIES(halite pthread)
