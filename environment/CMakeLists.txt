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

# Use git describe as the version for the executable, based on code from
# http://eatmyrandom.blogspot.com/2010/06/automate-version-numbering-using-git.html
find_package(Git)
if( GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git" )

execute_process(COMMAND git describe --abbrev=3 --tags HEAD
                COMMAND sed -e "s/-/./g"
                OUTPUT_VARIABLE HALITE_VERSION
                OUTPUT_STRIP_TRAILING_WHITESPACE)
# remove leading "v"
string(REGEX REPLACE "^v(.*)" "\\1" HALITE_VERSION ${HALITE_VERSION})
execute_process( COMMAND bash -c "echo \"${HALITE_VERSION}\" > ${CMAKE_SOURCE_DIR}/version" )


else( GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git" )

execute_process(COMMAND cat ${CMAKE_SOURCE_DIR}/version
                OUTPUT_VARIABLE HALITE_VERSION
                OUTPUT_STRIP_TRAILING_WHITESPACE)
string(CONCAT HALITE_VERSION ${HALITE_VERSION} ".src")

endif( GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git" )
message("Executable version ${HALITE_VERSION}")

configure_file(
    "${CMAKE_SOURCE_DIR}/version.hpp.in"
    "${CMAKE_SOURCE_DIR}/version.hpp"
    )

foreach(dir ${dirs})
    file(GLOB_RECURSE SOURCE ${dir}/*.[ch]*)
    set(SOURCE_FILES "${SOURCE_FILES}" ${SOURCE})
endforeach()

include_directories(${CMAKE_SOURCE_DIR})
set(SOURCE_FILES "${SOURCE_FILES}" main.cpp)

add_executable(halite ${SOURCE_FILES})

if (APPLE)
    # No static linkage here - https://stackoverflow.com/questions/5259249/creating-static-mac-os-x-c-build
    target_link_libraries(halite pthread)
else()
    # On Linux, make sure to link using the whole-archive option
    # See: https://stackoverflow.com/questions/35116327
    target_link_libraries(halite -Wl,--whole-archive pthread -Wl,--no-whole-archive -static)
endif()
