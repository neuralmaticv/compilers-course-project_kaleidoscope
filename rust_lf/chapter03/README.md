
## Chapter 3: Code generation to LLVM IR

[Link for tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html)  


```bash
clang++ -g -O3 main.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o main
```
