cmake_minimum_required(VERSION 3.13)
project(closure_test)

set(CMAKE_CXX_STANDARD 17)

include_directories(.)

add_compile_options(-Wall)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
set(install_gtest OFF)
set(install_gmock OFF)
set(build_gmock ON)

add_subdirectory(./googletest)

add_executable(closure_test
		closure.h
        test.cpp)

target_compile_options(closure_test PUBLIC -g)

target_link_libraries(closure_test gmock gtest gtest_main)

add_test(NAME closure_test COMMAND closure_test)
