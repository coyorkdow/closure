cmake_minimum_required(VERSION 3.9)
project(closure_test)

if (NOT DEFINED CMAKE_CXX_STANDARD)
    message(STATUS "The CPP standard is not set. Default: C++14")
    set(CMAKE_CXX_STANDARD 14)
else ()
    message(STATUS "Checking the CPP standard. C++${CMAKE_CXX_STANDARD}")
    if (CMAKE_CXX_STANDARD LESS 14)
        message(FATAL_ERROR "C++14 or higher is required.")
    endif ()
endif ()

include_directories(.)

add_compile_options(-Wall)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
set(install_gtest OFF)
set(install_gmock OFF)
set(build_gmock ON)

add_subdirectory(./googletest)

list(APPEND closure_src
        closure.hpp
        closure/traits.hpp
        closure/placeholders.hpp
        closure/util.hpp
        closure/bind.hpp
        closure/trivial_tuple.hpp
        )

add_executable(closure_test
        ${closure_src}
        test/test.cpp)

target_compile_options(closure_test PUBLIC -g)

target_link_libraries(closure_test gmock gtest gtest_main)

add_test(NAME closure_test COMMAND closure_test)
