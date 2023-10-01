clang++ -Xlinker --export-dynamic  -g main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main


use putchard(c:double);

fn printstar(n:double) {for i = 1.0, i < n, 1.0 in putchard(42)};


clang++ -g toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toy


extern putchard(c);

def printstar(n) for i = 1, i < n, 1.0 in putchard(42);
