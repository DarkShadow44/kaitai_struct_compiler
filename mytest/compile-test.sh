#!/bin/bash

gcc -I ../../runtime/c *.c ../../runtime/c/*.c -o test.app
#g++ -I ../../runtime/cpp_stl *.cpp ../../runtime/cpp_stl/*.cpp -o test.app
