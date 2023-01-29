include_directories(..)

add_executable(closure_test
        ../closure.hpp
        ../closure/traits.hpp
        ../closure/placeholders.hpp
        ../closure/util.hpp
        ../closure/bind.hpp
        test.cpp)

target_compile_options(closure_test PUBLIC -g)
