clang++ -g main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main

clang++ -Xlinker --export-dynamic -g main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main


fn printdensity(d:double) {if d > 8.0 { putchard(32) } else if d > 4.0 then putchard(46) else if d > 2.0 then   putchard(43) else   putchard(42) };