cmake_minimum_required(VERSION 3.9)
project(closure_example)


add_compile_options(-Wall)

add_executable(example example.cpp)
target_link_libraries(example closure)
