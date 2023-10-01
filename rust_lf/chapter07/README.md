clang++ -Xlinker --export-dynamic -g new_main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o main
