cmake_minimum_required(VERSION 3.2)

project("henk")

file(GLOB SRC *.cpp)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
add_definitions("-I/home/roland/projects/anydsl/thorin/src")
add_executable(henk ${SRC})
