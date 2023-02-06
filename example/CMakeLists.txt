cmake_minimum_required(VERSION 3.9)
project(closure_example)

if (NOT DEFINED CMAKE_CXX_STANDARD)
    message(STATUS "The CPP standard is not set. Default: C++14")
    set(CMAKE_CXX_STANDARD 14)
else ()
    message(STATUS "Checking the CPP standard. C++${CMAKE_CXX_STANDARD}")
    if (CMAKE_CXX_STANDARD LESS 14)
        message(FATAL_ERROR "C++14 or higher is required.")
    endif ()
endif ()

include_directories(..)

add_compile_options(-Wall)

list(APPEND closure_src
        ../closure.hpp
        ../closure/traits.hpp
        ../closure/placeholders.hpp
        ../closure/util.hpp
        ../closure/bind.hpp
        )

add_executable(example
        ${closure_src}
        example.cpp)
