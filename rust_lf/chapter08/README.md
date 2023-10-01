clang++ -Xlinker --export-dynamic -g -O3 main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o main



clag++ test.cpp output.o -o main

./test