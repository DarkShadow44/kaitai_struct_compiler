#!/bin/bash

gcc -I ../../runtime/c *.c ../../runtime/c/*.c -o test.app -Wall -Werror
#g++ -I ../../runtime/cpp_stl *.cpp ../../runtime/cpp_stl/*.cpp -o test.app
